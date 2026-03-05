import { createServer } from 'node:http';
import { access, readFile } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const rootDir = path.resolve(__dirname, '..');
const fixtureDir = path.resolve(rootDir, '../mimium-test/tests/mmm');
const stdlibDir = path.resolve(rootDir, '../../../lib');
const port = Number(process.env.PORT || 4173);
const mockLibPrefix = '/raw.githubusercontent.com/mimium-org/mimium-rs/';
const mockGithubTreePrefix = '/github.com/mimium-org/mimium-rs/';

const mockLibRequestCounts = new Map();

const contentTypes = new Map([
  ['.html', 'text/html; charset=utf-8'],
  ['.js', 'text/javascript; charset=utf-8'],
  ['.mjs', 'text/javascript; charset=utf-8'],
  ['.cjs', 'text/javascript; charset=utf-8'],
  ['.wasm', 'application/wasm'],
  ['.json', 'application/json; charset=utf-8'],
  ['.css', 'text/css; charset=utf-8'],
  ['.map', 'application/json; charset=utf-8']
]);

function resolveFilePath(requestPath) {
  const decodedPath = decodeURIComponent(requestPath.split('?')[0]);

  if (decodedPath.startsWith(mockGithubTreePrefix)) {
    const suffix = decodedPath.slice(mockGithubTreePrefix.length);
    const [mode, tag, ...rest] = suffix.split('/');
    const fileName = rest.length === 2 && rest[0] === 'lib' ? rest[1] : '';
    if (!tag || !fileName || fileName.includes('/')) {
      return null;
    }
    if (mode !== 'tree' && mode !== 'blob') {
      return null;
    }
    const absoluteStdlibPath = path.resolve(stdlibDir, fileName);
    if (!absoluteStdlibPath.startsWith(stdlibDir)) {
      return null;
    }
    return {
      filePath: absoluteStdlibPath,
      mockLibKey: `${tag}/${fileName}`
    };
  }

  if (decodedPath.startsWith(mockLibPrefix)) {
    const suffix = decodedPath.slice(mockLibPrefix.length);
    const [tag, ...rest] = suffix.split('/');
    const fileName = rest.length === 2 && rest[0] === 'lib' ? rest[1] : '';
    if (!tag || !fileName || fileName.includes('/')) {
      return null;
    }
    const absoluteStdlibPath = path.resolve(stdlibDir, fileName);
    if (!absoluteStdlibPath.startsWith(stdlibDir)) {
      return null;
    }
    return {
      filePath: absoluteStdlibPath,
      mockLibKey: `${tag}/${fileName}`
    };
  }

  if (requestPath.startsWith('/fixtures/')) {
    const fixturePath = requestPath.replace('/fixtures/', '');
    const absoluteFixturePath = path.resolve(fixtureDir, fixturePath);
    if (!absoluteFixturePath.startsWith(fixtureDir)) {
      return null;
    }
    return { filePath: absoluteFixturePath };
  }

  const normalizedPath = requestPath === '/' ? '/e2e/index.html' : requestPath;
  const normalizedDecodedPath = decodeURIComponent(normalizedPath.split('?')[0]);
  const absolutePath = path.resolve(rootDir, `.${normalizedDecodedPath}`);
  if (!absolutePath.startsWith(rootDir)) {
    return null;
  }
  return { filePath: absolutePath };
}

function mockLibStatsBody() {
  const requests = Object.fromEntries(mockLibRequestCounts.entries());
  const total = [...mockLibRequestCounts.values()].reduce((sum, count) => sum + count, 0);
  return JSON.stringify({ requests, total });
}

const server = createServer(async (req, res) => {
  const method = req.method || 'GET';
  if (method !== 'GET' && method !== 'HEAD') {
    res.writeHead(405, { 'Content-Type': 'text/plain; charset=utf-8' });
    res.end('Method Not Allowed');
    return;
  }

  const urlPath = decodeURIComponent((req.url || '/').split('?')[0]);

  if (urlPath === '/mock-lib-reset') {
    mockLibRequestCounts.clear();
    res.writeHead(200, {
      'Content-Type': 'application/json; charset=utf-8',
      'Cache-Control': 'no-store'
    });
    if (method === 'HEAD') {
      res.end();
      return;
    }
    res.end(mockLibStatsBody());
    return;
  }

  if (urlPath === '/mock-lib-stats') {
    res.writeHead(200, {
      'Content-Type': 'application/json; charset=utf-8',
      'Cache-Control': 'no-store'
    });
    if (method === 'HEAD') {
      res.end();
      return;
    }
    res.end(mockLibStatsBody());
    return;
  }

  const resolved = resolveFilePath(req.url || '/');
  if (!resolved) {
    res.writeHead(403, { 'Content-Type': 'text/plain; charset=utf-8' });
    res.end('Forbidden');
    return;
  }

  try {
    const { filePath, mockLibKey } = resolved;
    await access(filePath);
    if (mockLibKey) {
      const previousCount = mockLibRequestCounts.get(mockLibKey) || 0;
      mockLibRequestCounts.set(mockLibKey, previousCount + 1);
    }
    const ext = path.extname(filePath);
    const contentType = contentTypes.get(ext) || 'application/octet-stream';
    const body = await readFile(filePath);
    res.writeHead(200, {
      'Content-Type': contentType,
      'Cache-Control': 'no-store'
    });
    if (method === 'HEAD') {
      res.end();
      return;
    }
    res.end(body);
  } catch {
    res.writeHead(404, { 'Content-Type': 'text/plain; charset=utf-8' });
    res.end('Not Found');
  }
});

server.listen(port, '127.0.0.1', () => {
  console.log(`mimium-web e2e server listening on http://127.0.0.1:${port}`);
});
