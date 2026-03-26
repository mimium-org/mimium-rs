import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { expect, test } from '@playwright/test';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const fixtureDir = path.resolve(__dirname, '../../mimium-test/tests/mmm');
const metaPrefix = '// @test ';
const wasmUnsupportedFixtures = new Set([
  // These fixtures currently overflow the host JS stack in browser wasm during
  // compiler-side evaluation or pronoun conversion.
  'array_primitives_tuple_runtime.mmm',
  'fdn_rev_default_record_regression.mmm',
  'imported_core_generic_nested_array.mmm',
  'macro_quote_imported_global_function.mmm',
  'string_primitives.mmm'
]);

function getWebFixtureList() {
  return fs
    .readdirSync(fixtureDir)
    .filter((name) => name.endsWith('.mmm'))
    .filter((name) => {
      if (wasmUnsupportedFixtures.has(name)) {
        return false;
      }
      const source = fs.readFileSync(path.join(fixtureDir, name), 'utf8');
      const metaLine = source
        .split('\n')
        .slice(0, 8)
        .map((line) => line.trim())
        .find((line) => line.startsWith(metaPrefix));
      if (!metaLine) {
        return false;
      }
      const meta = JSON.parse(metaLine.slice(metaPrefix.length));
      return meta.web === true;
    })
    .sort();
}

const webFixtures = getWebFixtureList();

async function getMockLibStats(page) {
  return page.evaluate(async () => {
    const response = await fetch('/mock-lib-stats', { cache: 'no-store' });
    if (!response.ok) {
      throw new Error(`Failed to get mock lib stats: ${response.status}`);
    }
    return response.json();
  });
}

async function resetMockLibStats(page) {
  await page.evaluate(async () => {
    const response = await fetch('/mock-lib-reset', { cache: 'no-store' });
    if (!response.ok) {
      throw new Error(`Failed to reset mock lib stats: ${response.status}`);
    }
    await response.json();
  });
}

test.describe('web fixtures', () => {
  test('has at least one web fixture', () => {
    expect(webFixtures.length).toBeGreaterThan(0);
  });

  webFixtures.forEach((fixtureName) => {
    test(`runs ${fixtureName}`, async ({ page }) => {
      await page.goto('/e2e/index.html');
      await page.waitForFunction(
        () =>
          typeof window.runFixtureIntegration === 'function' ||
          window.__e2e_boot_error !== null
      );
      const bootError = await page.evaluate(() => window.__e2e_boot_error);
      expect(bootError).toBeNull();

      const result = await page.evaluate(
        async (name) => await window.runFixtureIntegration(name),
        fixtureName
      );
      expect(result.samples.length).toBe(result.expected.length);
      const tolerance = Math.max(result.tol || 0, 1e-5);
      result.samples.forEach((sample, index) => {
        expect(Math.abs(sample - result.expected[index])).toBeLessThanOrEqual(tolerance);
      });
    });
  });
});

test.describe('stdlib preload with temporary base URL', () => {
  test('fetches from mock server once and reuses cache on second preload', async ({ page }) => {
    await page.goto('/e2e/index.html');
    await page.waitForFunction(
      () =>
        typeof window.preloadStdlibFromBaseUrl === 'function' ||
        window.__e2e_boot_error !== null
    );
    const bootError = await page.evaluate(() => window.__e2e_boot_error);
    expect(bootError).toBeNull();

    await resetMockLibStats(page);

    const uniqueTag = `e2e-opfs-${Date.now()}`;
    const baseUrl = `${new URL('/', page.url()).toString()}raw.githubusercontent.com/mimium-org/mimium-rs/${uniqueTag}/lib/`;

    await page.evaluate(async (url) => {
      await window.preloadStdlibFromBaseUrl({ baseUrl: url, clearVirtualCache: true });
    }, baseUrl);

    const firstStats = await getMockLibStats(page);
    const firstEntries = Object.entries(firstStats.requests).filter(([key]) =>
      key.startsWith(`${uniqueTag}/`)
    );
    expect(firstEntries.length).toBeGreaterThan(0);
    expect(firstStats.total).toBeGreaterThan(0);

    await page.evaluate(async (url) => {
      await window.preloadStdlibFromBaseUrl({ baseUrl: url, clearVirtualCache: true });
    }, baseUrl);

    const secondStats = await getMockLibStats(page);
    expect(secondStats.total).toBe(firstStats.total);
  });

  test('accepts github tree URL and resolves it to raw content fetches', async ({ page }) => {
    await page.goto('/e2e/index.html');
    await page.waitForFunction(
      () =>
        typeof window.preloadStdlibFromBaseUrl === 'function' ||
        window.__e2e_boot_error !== null
    );
    const bootError = await page.evaluate(() => window.__e2e_boot_error);
    expect(bootError).toBeNull();

    await resetMockLibStats(page);

    const uniqueTag = `e2e-tree-${Date.now()}`;
    const treeBaseUrl = `${new URL('/', page.url()).toString()}github.com/mimium-org/mimium-rs/tree/${uniqueTag}/lib/`;

    await page.evaluate(async (url) => {
      await window.preloadStdlibFromBaseUrl({ baseUrl: url, clearVirtualCache: true });
    }, treeBaseUrl);

    const stats = await getMockLibStats(page);
    const tagEntries = Object.entries(stats.requests).filter(([key]) =>
      key.startsWith(`${uniqueTag}/`)
    );
    expect(tagEntries.length).toBeGreaterThan(0);
    expect(stats.total).toBeGreaterThan(0);
  });
});

test.describe('browser network module loading', () => {
  test('loads external module file over network during compile', async ({ page }) => {
    const requestedUrls = [];
    page.on('request', (request) => {
      requestedUrls.push(request.url());
    });

    await page.goto('/e2e/index.html');
    await page.waitForFunction(
      () =>
        typeof window.runFixtureIntegrationWithNetwork === 'function' ||
        window.__e2e_boot_error !== null
    );
    const bootError = await page.evaluate(() => window.__e2e_boot_error);
    expect(bootError).toBeNull();

    const result = await page.evaluate(async () => {
      return window.runFixtureIntegrationWithNetwork({
        fixtureName: 'module_external_use.mmm',
        moduleBaseUrl: `${location.origin}/fixtures/`
      });
    });

    expect(result.samples.length).toBe(result.expected.length);
    const tolerance = Math.max(result.tol || 0, 1e-5);
    result.samples.forEach((sample, index) => {
      expect(Math.abs(sample - result.expected[index])).toBeLessThanOrEqual(tolerance);
    });

    const moduleRequests = requestedUrls.filter((url) =>
      url.endsWith('/fixtures/module_external_math.mmm')
    );
    expect(moduleRequests.length).toBeGreaterThan(0);
  });
});
