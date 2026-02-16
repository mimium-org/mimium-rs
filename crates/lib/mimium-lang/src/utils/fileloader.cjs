let fs = null;
const isNode = typeof process !== "undefined" && !!(process.versions && process.versions.node);
if (isNode) {
  fs = require("fs");
}

const DEFAULT_GITHUB_LIB_BASE = "https://raw.githubusercontent.com/mimium-org/mimium-rs/";
const DEFAULT_GITHUB_TAG = "dev";
const LIB_FILES = [
  "core.mmm",
  "delay.mmm",
  "env.mmm",
  "filter.mmm",
  "math.mmm",
  "noise.mmm",
  "osc.mmm",
  "reactive.mmm",
  "reverb.mmm",
];

const memoryCache = new Map();
let lastPreloadBaseUrl = "";

function normalizePath(path) {
  if (typeof path !== "string") {
    return "";
  }
  const slash = path.replace(/\\/g, "/").trim();
  const withoutPrefix = slash
    .replace(/^\/+/, "")
    .replace(/^\.\//, "")
    .replace(/^lib\//, "");
  const collapsed = withoutPrefix
    .split("/")
    .filter((seg) => seg.length > 0 && seg !== ".")
    .join("/");
  return collapsed;
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
    return fs.readFileSync(path, { encoding: "utf8" });
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
exports.read_file = read_file;

function get_env(key) {
  if (isNode) {
    return process.env[key];
  }
  return undefined;
}
exports.get_env = get_env;

async function getOpfsLibDirectory() {
  const cacheDirName = getCacheDirName();
  if (!globalThis.navigator || !navigator.storage || !navigator.storage.getDirectory) {
    return null;
  }
  const root = await navigator.storage.getDirectory();
  return root.getDirectoryHandle(cacheDirName, { create: true });
}

function getTagFromBaseUrl(baseUrl) {
  const normalized = (baseUrl || "").replace(/\/+$/, "");
  const marker = "raw.githubusercontent.com/mimium-org/mimium-rs/";
  const markerIdx = normalized.indexOf(marker);
  if (markerIdx < 0) {
    return DEFAULT_GITHUB_TAG;
  }
  const suffix = normalized.slice(markerIdx + marker.length);
  const tag = suffix.split("/")[0];
  return tag || DEFAULT_GITHUB_TAG;
}

function getCacheDirName() {
  return `mimium-lib-${getTagFromBaseUrl(globalThis.__mimium_lib_base_url || "")}`;
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

async function fetchLibFile(baseUrl, filename) {
  const url = `${baseUrl}${filename}`;
  const response = await fetch(url, { cache: "no-cache" });
  if (!response.ok) {
    throw new Error(`Failed to fetch ${url}: ${response.status} ${response.statusText}`);
  }
  return response.text();
}

async function preload_mimium_lib_cache(base_url) {
  const baseUrlCandidate =
    base_url && base_url.length > 0
      ? base_url
      : `${DEFAULT_GITHUB_LIB_BASE}${DEFAULT_GITHUB_TAG}/lib/`;
  const baseUrl = baseUrlCandidate.endsWith("/") ? baseUrlCandidate : `${baseUrlCandidate}/`;
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
exports.preload_mimium_lib_cache = preload_mimium_lib_cache;

function __mimium_test_put_cache(path, content) {
  putMemoryAliases(path, content);
}
exports.__mimium_test_put_cache = __mimium_test_put_cache;

function __mimium_test_clear_cache() {
  memoryCache.clear();
  lastPreloadBaseUrl = "";
  delete globalThis.__mimium_lib_base_url;
}
exports.__mimium_test_clear_cache = __mimium_test_clear_cache;

function __mimium_test_get_last_preload_base_url() {
  return lastPreloadBaseUrl;
}
exports.__mimium_test_get_last_preload_base_url = __mimium_test_get_last_preload_base_url;