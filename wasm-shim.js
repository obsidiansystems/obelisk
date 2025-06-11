import {
  WASI,
  OpenFile,
  File,
  ConsoleStdout,
} from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.4.1/+esm";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

// -T is for RTS stats, -c to use compacting gc for the larges generation in an attempt to not grow max use too much
const args = ["frontend.wasm", "+RTS", "-H64m", "-c", "-T", "-RTS"];
const env = [];
const fds = [
  new OpenFile(new File(new Uint8Array(), { readonly: true })),
  ConsoleStdout.lineBuffered((msg) =>
    console.info(`[frontend.wasm] ${msg}`)
  ),
  ConsoleStdout.lineBuffered((msg) =>
    console.error(`[frontend.wasm] ${msg}`)
  ),
];
const options = { debug: false };
const wasi = new WASI(args, env, fds, options);

const instance_exports = {};
const { instance } = await WebAssembly.instantiateStreaming(
  fetch("./ghcjs/frontend.wasm"),
  {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
  }
);
Object.assign(instance_exports, instance.exports);

wasi.initialize(instance);

instance_exports.hsMain();
