import { WASI } from './wasi';
import term_wasm_name from './generated/wasm-terminal/wasm-terminal.wasm';
const term_wasm_path = `/static/dist/${term_wasm_name}`;

export class BaseTerminal {
  instance?: WebAssembly.Instance;
  wasi: WASI;

  constructor() {
    this.wasi = new WASI();
  }

  process_data(data: Uint8Array, finalise = false) {
    if (this.instance === undefined) {
      throw Error('Not initialised');
    }

    const memory = this.instance.exports.memory as WebAssembly.Memory;

    const do_decode = this.instance.exports.do_decode as (ptr: number, cnt: number) => void;
    const alloc = this.instance.exports.alloc as (count: number) => number;
    const dealloc = this.instance.exports.dealloc as (ptr: number) => void;

    const ptr = alloc(data.length);

    try {
      const wasmData = new Uint8Array(memory.buffer, ptr, data.length)
      wasmData.set(data)
      do_decode(ptr, data.length);
    } finally {
      dealloc(ptr);
    }

    if (finalise)
      this.process_done();
  }

  process_done() {
    if (this.instance === undefined) {
      throw Error('Not initialised');
    }

    const push_changes = this.instance.exports.push_changes as () => void;
    push_changes();
  }

  set_size(width: number, height: number) {
    throw Error('Not implemented')
  }

  set_scroll_change(change: number) {
    throw Error('Not implemented')
  }

  set_glyph(x: number, y: number, code: number,
      fg_r: number, fg_g: number, fg_b: number,
      bg_r: number, bg_g: number, bg_b: number)
  {
    throw Error('Not implemented')
  }

  set_cursor(x: number, y: number)
  {
    throw Error('Not implemented');
  }

  bracketed_paste_mode(): boolean
  {
    if (this.instance === undefined)
      throw Error('Not initialized');

    const bp = this.instance.exports.bracketed_paste as () => number;
    return bp() !== 0;
  }

  async init() {
    const importObject = {
      wasi_snapshot_preview1: this.wasi.get_imports(),
      terminal: {
        set_size: this.set_size.bind(this),
        set_scroll_change: this.set_scroll_change.bind(this),
        set_glyph: this.set_glyph.bind(this),
        set_cursor: this.set_cursor.bind(this),
      },
    }

    const terminalModule =
      await WebAssembly.compileStreaming(fetch(term_wasm_path));

    this.instance =
      await WebAssembly.instantiate(terminalModule, importObject);
    this.wasi.set_instance(this.instance);
    const initialize = this.instance.exports._initialize as CallableFunction;
    initialize();
  }
}

