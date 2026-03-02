import { expect, test } from '@playwright/test';

test('mod/use external module works in browser runtime', async ({ page }) => {
  await page.goto('/e2e/index.html');
  await page.waitForFunction(
    () =>
      typeof window.runModuleUseIntegration === 'function' ||
      window.__e2e_boot_error !== null
  );
  const bootError = await page.evaluate(() => window.__e2e_boot_error);
  expect(bootError).toBeNull();
  const value = await page.evaluate(async () => window.runModuleUseIntegration());
  expect(value).toBeCloseTo(12, 5);
});
