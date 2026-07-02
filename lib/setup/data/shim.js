(function() {
  var base = new URL(".", document.currentScript.src).href;
  (async function() {
    const { WASI, OpenFile, File, ConsoleStdout } = await import(base + "wasi-shim.js");
    const { default: ghc_wasm_jsffi } = await import(base + "ghc_wasm_jsffi.js");

    const args = [];
    const env = ["GHCRTS=-H64m"];
    const fds = [
      new OpenFile(new File([])),
      ConsoleStdout.lineBuffered(msg => console.log(`[WASI stdout] ${msg}`)),
      ConsoleStdout.lineBuffered(msg => console.warn(`[WASI stderr] ${msg}`)),
    ];
    const wasi = new WASI(args, env, fds, { debug: false });

    const instance_exports = {};
    const response = await fetch(base + "frontend.wasm");
    const bytes = await response.arrayBuffer();
    const { instance } = await WebAssembly.instantiate(bytes,
      { wasi_snapshot_preview1: wasi.wasiImport,
        ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports) }
    );
    Object.assign(instance_exports, instance.exports);
    wasi.initialize(instance);
    await instance.exports.hs_start();
  })();
})();
