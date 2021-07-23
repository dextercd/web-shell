import { errno } from './errno'

export interface WASICompatibleInstance extends WebAssembly.Instance {
  exports: {
    memory: WebAssembly.Memory;
  }
}

export class WASI {
  instance?: WASICompatibleInstance;
  args = ['term']

  get_imports() {
    return {
      args_get: this.args_get.bind(this),
      args_sizes_get: this.args_sizes_get.bind(this),
      environ_get: this.environ_get.bind(this),
      environ_sizes_get: this.environ_sizes_get.bind(this),
      clock_time_get: this.clock_time_get.bind(this),
      fd_close: this.fd_close.bind(this),
      fd_fdstat_get: this.fd_fdstat_get.bind(this),
      fd_fdstat_set_flags: this.fd_fdstat_set_flags.bind(this),
      fd_prestat_get: this.fd_prestat_get.bind(this),
      fd_prestat_dir_name: this.fd_prestat_dir_name.bind(this),
      fd_read: this.fd_read.bind(this),
      fd_seek: this.fd_seek.bind(this),
      fd_write: this.fd_write.bind(this),
      path_open: this.path_open.bind(this),
      proc_exit: this.proc_exit.bind(this),
    }
  }

  set_instance(instance: WebAssembly.Instance) {
    if (!(instance.exports.memory instanceof WebAssembly.Memory)) {
      throw Error('Missing memory export.');
    }
    this.instance = instance as WASICompatibleInstance;
  }

  has_instance(): this is { instance: WebAssembly.Instance } {
    return this.instance !== undefined;
  }

  ensure_instance(): asserts this is { instance: WebAssembly.Instance } {
    if (!this.has_instance())
      throw Error('Function called without instance!');
  }

  args_sizes_get(retBuf: number) {
    this.ensure_instance();

    const memory = this.instance.exports.memory as WebAssembly.Memory;
    const dataview = new DataView(memory.buffer)

    dataview.setUint32(retBuf, this.args.length, true);
    const bufferSize = this.args.length + this.args.map(arg => arg.length).reduce((a, b) => a + b);
    dataview.setUint32(retBuf + 4, bufferSize, true);

    return errno.success;
  }

  args_get(argv: number, argvBuf: number, retBuf: number) {
    this.ensure_instance();

    const encoder = new TextEncoder();

    const memory = this.instance.exports.memory as WebAssembly.Memory;
    const dataview = new DataView(memory.buffer)

    for (const arg of this.args) {
      const argBinary = encoder.encode(arg)
      const textBuffer = new Uint8Array(memory.buffer);
      textBuffer.set(argBinary, argvBuf)
      textBuffer.set([0], argvBuf + argBinary.length)

      dataview.setUint32(argv, argvBuf, true);
      argv += 4
      argvBuf += argBinary.length + 1
    }

    return errno.success;
  }

  environ_get() {
    throw Error('environ_get')
  }

  environ_sizes_get() {
    throw Error('environ_sizes_get')
  }

  clock_time_get(clockId: number, precision: number, retBuf: number) {
    const milliseconds = new Date().getTime();
    const nanoseconds = BigInt(milliseconds) * 1000000n
    return errno.success;
  }

  fd_close() {
    throw Error('fd_close')
  }

  fd_fdstat_get(filde: number, retBuf: number) {
    console.log(filde);
    //throw Error('fd_fdstat_get')
    return errno.badf;
  }

  fd_fdstat_set_flags() {

    throw Error('fd_fdstat_set_flags')
  }

  fd_prestat_get(filde: number, retBuf: number) {
    this.ensure_instance();

    console.log('fd_prestat_get');
    console.log(filde);

    return errno.badf;
  }

  fd_prestat_dir_name(filde: number, path_ptr: number, path_len: number, retBuf: number) {
    console.log('fd_prestat_dir_name')
    console.log(filde)
    console.log(path_ptr)
    console.log(path_len)
    throw Error('TODO');
  }

  fd_read() {
    throw Error('fd_read')
  }

  fd_seek() {
    throw Error('fd_seek')
  }

  fd_write(fildes: number, iovs: number, iocnt: number, retBuf: number) {
    this.ensure_instance();

    const memory = this.instance.exports.memory as WebAssembly.Memory;
    const dataview = new DataView(memory.buffer);

    let written = 0;

    for (let i = 0; i != iocnt; ++i) {
      const textStart = dataview.getUint32(iovs + i * 8, true);
      const textLength = dataview.getUint32(iovs + 4 + i * 8, true);
      const textBuffer = new Uint8Array(memory.buffer).slice(textStart, textStart + textLength);
      const str = new TextDecoder().decode(textBuffer);
      if (str.length !== 0)
        console.log(str);

      written += textLength;
    }

    dataview.setUint32(retBuf, written, true)

    return errno.success;
  }

  path_open() {
    throw Error('path_open')
  }

  proc_exit(exitCode: number) {
    throw Error(`Program stopped with code: ${exitCode}`);
  }
}
