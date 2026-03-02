import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { expect, test } from '@playwright/test';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const fixtureDir = path.resolve(__dirname, '../../mimium-test/tests/mmm');
const metaPrefix = '// @test ';

function getWebFixtureList() {
  return fs
    .readdirSync(fixtureDir)
    .filter((name) => name.endsWith('.mmm'))
    .filter((name) => {
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
