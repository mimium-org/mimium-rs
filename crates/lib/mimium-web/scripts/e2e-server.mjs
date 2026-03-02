import { createServer } from 'node:http';
import { access, readFile } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const rootDir = path.resolve(__dirname, '..');
const fixtureDir = path.resolve(rootDir, '../mimium-test/tests/mmm');
const port = Number(process.env.PORT || 4173);

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
  if (requestPath.startsWith('/fixtures/')) {
    const fixturePath = requestPath.replace('/fixtures/', '');
    const absoluteFixturePath = path.resolve(fixtureDir, fixturePath);
    if (!absoluteFixturePath.startsWith(fixtureDir)) {
      return null;
    }
    return absoluteFixturePath;
  }

  const normalizedPath = requestPath === '/' ? '/e2e/index.html' : requestPath;
  const decodedPath = decodeURIComponent(normalizedPath.split('?')[0]);
  const absolutePath = path.resolve(rootDir, `.${decodedPath}`);
  if (!absolutePath.startsWith(rootDir)) {
    return null;
  }
  return absolutePath;
}

const server = createServer(async (req, res) => {
  const method = req.method || 'GET';
  if (method !== 'GET' && method !== 'HEAD') {
    res.writeHead(405, { 'Content-Type': 'text/plain; charset=utf-8' });
    res.end('Method Not Allowed');
    return;
  }

  const filePath = resolveFilePath(req.url || '/');
  if (!filePath) {
    res.writeHead(403, { 'Content-Type': 'text/plain; charset=utf-8' });
    res.end('Forbidden');
    return;
  }

  try {
    await access(filePath);
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
