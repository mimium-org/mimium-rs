import { defineConfig } from '@playwright/test';

export default defineConfig({
  testDir: './e2e',
  timeout: 30_000,
  fullyParallel: false,
  workers: 1,
  use: {
    headless: true,
    baseURL: 'http://127.0.0.1:4173'
  },
  webServer: {
    command: 'npm run build:wasm && node ./scripts/e2e-server.mjs',
    port: 4173,
    reuseExistingServer: true,
    timeout: 120_000
  }
});
