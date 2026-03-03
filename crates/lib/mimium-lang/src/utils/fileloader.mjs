let fs = null;
const isNode = typeof process !== 'undefined' && !!(process.versions && process.versions.node);
if (isNode) {
  if (typeof process.getBuiltinModule === 'function') {
    fs = process.getBuiltinModule('node:fs') ?? process.getBuiltinModule('fs');
  }
  if (!fs) {
    try {
      const requireOrNull = Function(
        'return typeof require !== "undefined" ? require : null;'
      )();
      if (requireOrNull) {
        fs = requireOrNull('node:fs');
      }
    } catch {
      fs = null;
    }
  }
}

const DEFAULT_GITHUB_LIB_BASE = 'https://raw.githubusercontent.com/mimium-org/mimium-rs/';
const DEFAULT_GITHUB_TAG = 'dev';
const LIB_FILES = [
  'core.mmm',
  'delay.mmm',
  'env.mmm',
  'filter.mmm',
  'math.mmm',
  'noise.mmm',
  'osc.mmm',
  'reactive.mmm',
  'reverb.mmm'
];

const memoryCache = new Map();
let lastPreloadBaseUrl = '';
let lastModulePreloadBaseUrl = '';

function normalizePath(path) {
  if (typeof path !== 'string') {
    return '';
  }
  const slash = path.replace(/\\/g, '/').trim();
  const withoutPrefix = slash.replace(/^\/+/, '').replace(/^\.\//, '').replace(/^lib\//, '');
  const collapsed = withoutPrefix
    .split('/')
    .filter((seg) => seg.length > 0 && seg !== '.')
    .join('/');
  return collapsed;
}

function normalizeBaseUrlCandidate(baseUrlCandidate) {
  if (!baseUrlCandidate || baseUrlCandidate.length === 0) {
    return '';
  }

  let resolved = baseUrlCandidate;
  if (typeof location !== 'undefined') {
    resolved = new URL(baseUrlCandidate, location.href).toString();
  }

  try {
    const parsed = new URL(resolved);
    if (parsed.hostname !== 'github.com') {
      return resolved;
    }

    const segments = parsed.pathname.split('/').filter((segment) => segment.length > 0);
    const [owner, repo, mode, ref, ...rest] = segments;
    if (!owner || !repo || !mode || !ref) {
      return resolved;
    }
    if (mode !== 'tree' && mode !== 'blob') {
      return resolved;
    }

    const restPath = rest.join('/');
    const rawPath = restPath.length > 0 ? `/${owner}/${repo}/${ref}/${restPath}` : `/${owner}/${repo}/${ref}`;
    return `https://raw.githubusercontent.com${rawPath}`;
  } catch {
    return resolved;
  }
}

function has_network_api() {
  return typeof globalThis.fetch === 'function' || typeof XMLHttpRequest !== 'undefined';
}

function putMemoryAliases(filename, content) {
  const normalized = normalizePath(filename);
  if (!normalized) {
    return;
  }
  memoryCache.set(normalized, content);
  memoryCache.set(`./${normalized}`, content);
  memoryCache.set(`lib/${normalized}`, content);
  memoryCache.set(`/lib/${normalized}`, content);
}

function read_file(path) {
  if (isNode) {
    return fs.readFileSync(path, { encoding: 'utf8' });
  }

  const normalized = normalizePath(path);
  const cached =
    memoryCache.get(path) ??
    memoryCache.get(normalized) ??
    memoryCache.get(`./${normalized}`) ??
    memoryCache.get(`lib/${normalized}`) ??
    memoryCache.get(`/lib/${normalized}`);

  if (cached !== undefined) {
    return cached;
  }

  throw new Error(
    `Include target not found in browser cache: ${path}. Call preload_mimium_lib_cache() before compile().`
  );
}

function get_env(key) {
  if (isNode) {
    return process.env[key];
  }
  return undefined;
}

async function getOpfsLibDirectory() {
  const cacheDirName = getCacheDirName();
  if (!globalThis.navigator || !navigator.storage || !navigator.storage.getDirectory) {
    return null;
  }
  const root = await navigator.storage.getDirectory();
  return root.getDirectoryHandle(cacheDirName, { create: true });
}

function getTagFromBaseUrl(baseUrl) {
  const normalized = (baseUrl || '').replace(/\/+$/, '');
  const marker = 'raw.githubusercontent.com/mimium-org/mimium-rs/';
  const markerIdx = normalized.indexOf(marker);
  if (markerIdx < 0) {
    return DEFAULT_GITHUB_TAG;
  }
  const suffix = normalized.slice(markerIdx + marker.length);
  const tag = suffix.split('/')[0];
  return tag || DEFAULT_GITHUB_TAG;
}

function getCacheDirName() {
  return `mimium-lib-${getTagFromBaseUrl(globalThis.__mimium_lib_base_url || '')}`;
}

async function readFromOpfs(filename) {
  const dir = await getOpfsLibDirectory();
  if (!dir) {
    return null;
  }
  try {
    const handle = await dir.getFileHandle(filename, { create: false });
    const file = await handle.getFile();
    return await file.text();
  } catch {
    return null;
  }
}

async function writeToOpfs(filename, content) {
  const dir = await getOpfsLibDirectory();
  if (!dir) {
    return;
  }
  const handle = await dir.getFileHandle(filename, { create: true });
  const writable = await handle.createWritable();
  await writable.write(content);
  await writable.close();
}

async function requestText(url) {
  if (typeof globalThis.fetch === 'function') {
    const response = await globalThis.fetch(url, { cache: 'no-cache' });
    if (!response.ok) {
      throw new Error(`Failed to fetch ${url}: ${response.status} ${response.statusText}`);
    }
    return response.text();
  }

  if (typeof XMLHttpRequest !== 'undefined') {
    return new Promise((resolve, reject) => {
      const xhr = new XMLHttpRequest();
      xhr.open('GET', url, true);
      xhr.onreadystatechange = () => {
        if (xhr.readyState !== 4) {
          return;
        }
        if (xhr.status >= 200 && xhr.status < 300) {
          resolve(xhr.responseText);
          return;
        }
        reject(new Error(`Failed to fetch ${url}: ${xhr.status} ${xhr.statusText}`));
      };
      xhr.onerror = () => reject(new Error(`Failed to fetch ${url}: network error`));
      xhr.send();
    });
  }

  throw new Error(
    'Network API is unavailable: neither fetch nor XMLHttpRequest exists in this runtime.'
  );
}

async function fetchLibFile(baseUrl, filename) {
  const url = `${baseUrl}${filename}`;
  return requestText(url);
}

function collectDependencies(source) {
  const moduleDeps = [...source.matchAll(/^\s*mod\s+([A-Za-z_][A-Za-z0-9_]*)\s*$/gm)].map(
    (match) => `${match[1]}.mmm`
  );
  const includeDeps = [...source.matchAll(/^\s*include\(\s*"([^"]+)"\s*\)\s*$/gm)].map(
    (match) => match[1]
  );
  return [...new Set([...moduleDeps, ...includeDeps])];
}

function getModuleBaseUrl(base_url) {
  const candidate =
    base_url && base_url.length > 0
      ? base_url
      : globalThis.__mimium_module_base_url ||
        (typeof location !== 'undefined' ? new URL('.', location.href).toString() : '');
  if (!candidate) {
    return '';
  }
  const normalized = normalizeBaseUrlCandidate(candidate);
  return normalized.endsWith('/') ? normalized : `${normalized}/`;
}

async function fetchSourceFromBaseUrl(baseUrl, relativePath) {
  const url = new URL(relativePath, baseUrl).toString();
  return requestText(url);
}

async function preload_user_module_cache(source, base_url) {
  if (isNode) {
    return;
  }

  const baseUrl = getModuleBaseUrl(base_url);
  if (!baseUrl) {
    return;
  }
  globalThis.__mimium_module_base_url = baseUrl;
  lastModulePreloadBaseUrl = baseUrl;

  const pending = collectDependencies(source);
  const visited = new Set();

  while (pending.length > 0) {
    const depPath = pending.pop();
    const normalized = normalizePath(depPath || '');
    if (!normalized || visited.has(normalized)) {
      continue;
    }
    visited.add(normalized);

    const cached =
      memoryCache.get(depPath) ??
      memoryCache.get(normalized) ??
      memoryCache.get(`./${normalized}`) ??
      memoryCache.get(`lib/${normalized}`) ??
      memoryCache.get(`/lib/${normalized}`);
    if (cached !== undefined) {
      collectDependencies(cached).forEach((nestedDep) => pending.push(nestedDep));
      continue;
    }

    const fetched = await fetchSourceFromBaseUrl(baseUrl, depPath);
    putMemoryAliases(depPath, fetched);
    collectDependencies(fetched).forEach((nestedDep) => pending.push(nestedDep));
  }
}

async function preload_mimium_lib_cache(base_url) {
  const baseUrlCandidate =
    base_url && base_url.length > 0
      ? base_url
      : `${DEFAULT_GITHUB_LIB_BASE}${DEFAULT_GITHUB_TAG}/lib/`;
  const normalizedBaseUrl = normalizeBaseUrlCandidate(baseUrlCandidate);
  const baseUrl = normalizedBaseUrl.endsWith('/') ? normalizedBaseUrl : `${normalizedBaseUrl}/`;
  globalThis.__mimium_lib_base_url = baseUrl;
  lastPreloadBaseUrl = baseUrl;

  if (isNode) {
    return;
  }

  for (const filename of LIB_FILES) {
    const fromMemory = memoryCache.get(filename);
    if (fromMemory !== undefined) {
      putMemoryAliases(filename, fromMemory);
      continue;
    }

    const fromOpfs = await readFromOpfs(filename);
    if (fromOpfs !== null) {
      putMemoryAliases(filename, fromOpfs);
      continue;
    }

    const fetched = await fetchLibFile(baseUrl, filename);
    putMemoryAliases(filename, fetched);
    await writeToOpfs(filename, fetched);
  }
}

function __mimium_test_put_cache(path, content) {
  putMemoryAliases(path, content);
}

function __mimium_test_clear_cache() {
  memoryCache.clear();
  lastPreloadBaseUrl = '';
  lastModulePreloadBaseUrl = '';
  delete globalThis.__mimium_lib_base_url;
  delete globalThis.__mimium_module_base_url;
}

function __mimium_test_get_last_preload_base_url() {
  return lastPreloadBaseUrl;
}

function __mimium_test_get_last_module_preload_base_url() {
  return lastModulePreloadBaseUrl;
}

function export_virtual_file_cache_json() {
  const entries = [];
  const seen = new Set();
  for (const [key, value] of memoryCache.entries()) {
    const normalized = normalizePath(key);
    if (!normalized || seen.has(normalized)) {
      continue;
    }
    seen.add(normalized);
    entries.push([normalized, value]);
  }
  return JSON.stringify(entries);
}

function import_virtual_file_cache_json(payload) {
  const parsed = JSON.parse(payload);
  if (!Array.isArray(parsed)) {
    throw new Error('Invalid cache payload: expected array entries');
  }
  parsed.forEach((entry) => {
    if (!Array.isArray(entry) || entry.length !== 2) {
      throw new Error('Invalid cache payload entry');
    }
    const [path, content] = entry;
    if (typeof path !== 'string' || typeof content !== 'string') {
      throw new Error('Invalid cache payload types');
    }
    putMemoryAliases(path, content);
  });
}

function set_module_base_url(base_url) {
  const normalized = getModuleBaseUrl(base_url);
  if (!normalized) {
    delete globalThis.__mimium_module_base_url;
    return;
  }
  globalThis.__mimium_module_base_url = normalized;
}

export {
  read_file,
  get_env,
  has_network_api,
  preload_mimium_lib_cache,
  preload_user_module_cache,
  set_module_base_url,
  export_virtual_file_cache_json,
  import_virtual_file_cache_json,
  __mimium_test_put_cache,
  __mimium_test_clear_cache,
  __mimium_test_get_last_preload_base_url,
  __mimium_test_get_last_module_preload_base_url
};
