// Obelisk WASM bootstrap. Served as all.js; loads the GHC-compiled
// frontend.wasm that lives alongside it, together with its JSFFI bindings
// (ghc_wasm_jsffi.js) and the browser WASI polyfill (wasi-shim.js).
//
// Applications may override runtime settings by defining a global before
// this script runs:
//
//   globalThis.__obelisk_wasm = {
//     args: [],                 // WASI argv
//     env: ["GHCRTS=-H64m"],    // WASI environment
//     stdout: line => {...},    // line-buffered stdout sink
//     stderr: line => {...},    // line-buffered stderr sink
//   };
(function() {
  // Sibling assets are fetched relative to this script's own URL.
  // document.currentScript covers parser-inserted, deferred, and
  // dynamically-injected classic scripts; fall back to scanning script tags
  // for evaluation paths that don't set it (e.g. module loading or bundling).
  var currentSrc = document.currentScript && document.currentScript.src;
  if (!currentSrc) {
    var candidates = document.querySelectorAll("script[src]");
    for (var i = candidates.length - 1; i >= 0 && !currentSrc; i--) {
      if (/(^|\/)all\.js([?#]|$)/.test(candidates[i].src)) {
        currentSrc = candidates[i].src;
      }
    }
  }
  if (!currentSrc) {
    throw new Error("obelisk: cannot locate the all.js script tag to derive asset URLs");
  }
  var base = new URL(".", currentSrc).href;
  var opts = globalThis.__obelisk_wasm || {};
  (async function() {
    const { WASI, OpenFile, File, ConsoleStdout } = await import(base + "wasi-shim.js");
    const { default: ghc_wasm_jsffi } = await import(base + "ghc_wasm_jsffi.js");

    const args = opts.args || [];
    const env = opts.env || ["GHCRTS=-H64m"];
    const fds = [
      new OpenFile(new File([])),
      ConsoleStdout.lineBuffered(opts.stdout || (msg => console.log(`[WASI stdout] ${msg}`))),
      ConsoleStdout.lineBuffered(opts.stderr || (msg => console.warn(`[WASI stderr] ${msg}`))),
    ];
    const wasi = new WASI(args, env, fds, { debug: false });

    const instance_exports = {};
    const imports = {
      wasi_snapshot_preview1: wasi.wasiImport,
      ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
    };
    // Compile while downloading when the server sends the proper wasm MIME
    // type (obelisk's asset server does); otherwise buffer and instantiate.
    const response = await fetch(base + "frontend.wasm");
    const contentType = (response.headers.get("Content-Type") || "").split(";")[0].trim();
    let result;
    if (contentType === "application/wasm" && WebAssembly.instantiateStreaming) {
      result = await WebAssembly.instantiateStreaming(response, imports);
    } else {
      result = await WebAssembly.instantiate(await response.arrayBuffer(), imports);
    }
    const { instance } = result;
    Object.assign(instance_exports, instance.exports);
    wasi.initialize(instance);
    await instance.exports.hs_start();
  })();
})();
