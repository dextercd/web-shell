import { WASI } from './wasi'

(async () => {

const terminalModule =
  await WebAssembly.compileStreaming(fetch('/static/terminal.wasm'));

class BaseTerminal {
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
    this.instance =
      await WebAssembly.instantiate(terminalModule, importObject);
    this.wasi.set_instance(this.instance);
    const initialize = this.instance.exports._initialize as CallableFunction;
    initialize();
  }
}

class LineInProgress {
  spans: HTMLSpanElement[] = [];

  current_style = {
    fg_r: -1,
    fg_g: -1,
    fg_b: -1,

    bg_r: -1,
    bg_g: -1,
    bg_b: -1,
  }

  add_glyph(code: number,
      fg_r: number, fg_g: number, fg_b: number,
      bg_r: number, bg_g: number, bg_b: number)
  {
    if (code == 0)
      code = 0x20

    if (
      (bg_r != this.current_style.bg_r ||
       bg_r != this.current_style.bg_r ||
       bg_r != this.current_style.bg_r)

      ||

      (code != 0x20 && (
        fg_r != this.current_style.fg_r ||
          fg_g != this.current_style.fg_g ||
          fg_b != this.current_style.fg_b)))
    {
      const newSpan = document.createElement('span')
      newSpan.style.color = `#${fg_r.toString(16).padStart(2, '0')}${fg_g.toString(16).padStart(2, '0')}${fg_b.toString(16).padStart(2, '0')}`;
      newSpan.style.backgroundColor = `#${bg_r.toString(16).padStart(2, '0')}${bg_g.toString(16).padStart(2, '0')}${bg_b.toString(16).padStart(2, '0')}`;

      this.current_style.fg_r = fg_r;
      this.current_style.fg_g = fg_g;
      this.current_style.fg_b = fg_b;

      this.current_style.bg_r = bg_r;
      this.current_style.bg_g = bg_g;
      this.current_style.bg_b = bg_b;

      this.spans.push(newSpan)
    }

    const span = this.spans[this.spans.length - 1];
    span.textContent += String.fromCodePoint(code);
  }
}

class DOMTerminal extends BaseTerminal {
  width = 80;
  height = 24;
  cursorx = 0;
  cursory = 0;
  root_element: HTMLPreElement;
  base_element: HTMLDivElement;
  linesParentElement: HTMLDivElement;
  cursor_element: HTMLSpanElement;
  in_progress_lines: {[key: number]: LineInProgress} = {};
  enableCursorAnimation: boolean;

  constructor(root_element: HTMLPreElement) {
    super();

    // Fixed element translate animation is not working properly
    // on Chrome.  Disable for now.
    this.enableCursorAnimation = !navigator.userAgent.includes('Chrome');

    this.root_element = root_element;
    this.root_element.innerHTML = '';

    this.base_element = document.createElement('div');
    this.base_element.style.position = 'relative';

    this.linesParentElement = document.createElement('div')

    this.cursor_element = document.createElement('span');
    this.cursor_element.textContent = ' ';
    this.cursor_element.style.position = 'absolute';
    this.cursor_element.style.top = '0';
    this.cursor_element.style.left = '0';
    this.cursor_element.classList.add('cursor');

    this.root_element.append(this.base_element);
    this.base_element.append(this.linesParentElement);
    this.base_element.append(this.cursor_element);

    this.set_cursor(0, 0);
    this.setup_screen();
  }

  set_cursor(x: number, y: number) {
    if (this.enableCursorAnimation) {
      const smoothMoveClass = 'cursor-smooth-move';
      if (
          (this.cursory === y || this.cursorx === x) ||
          (Math.abs(this.cursory - y) <= 1 && Math.abs(this.cursorx - x) <= 1))
      {
        this.cursor_element.classList.add(smoothMoveClass);
      } else {
        this.cursor_element.classList.remove(smoothMoveClass);
      }
    }

    this.cursor_element.style.transform = `translate(${x}00%, ${y}00%)`

    this.cursorx = x;
    this.cursory = y;
  }

  set_size(width: number, height: number) {
    if (this.width != width || this.height != height) {
      this.width = width;
      this.height = height;
      this.setup_screen();
    }
  }

  setup_screen() {
    this.linesParentElement.innerHTML = '';

    for (let line = 0; line != this.height; ++line) {
      const lineSpan = document.createElement('span');
      lineSpan.style.display = 'block';
      lineSpan.style.backgroundColor = 'black';

      const glyphSpan = document.createElement('span')
      glyphSpan.textContent = ' '.repeat(this.width);

      lineSpan.appendChild(glyphSpan);
      this.linesParentElement.appendChild(lineSpan);
    }
  }

  scroll_content_up(count: number) {
    while(count--) {
      const first = this.linesParentElement.childNodes[0]
      this.linesParentElement.append(first)
    }
  }

  scroll_content_down(count: number) {
    while(count--) {
      const last = this.linesParentElement.children[this.linesParentElement.children.length - 1];
      this.linesParentElement.prepend(last)
    }
  }

  set_scroll_change(change: number) {
    if (change > 0)
      this.scroll_content_up(change);
    else if (change < 0)
      this.scroll_content_down(-change);
  }

  set_glyph(x: number, y: number, code: number,
      fg_r: number, fg_g: number, fg_b: number,
      bg_r: number, bg_g: number, bg_b: number)
  {
    if (!(y in this.in_progress_lines)) {
      this.in_progress_lines[y] = new LineInProgress()
    }

    this.in_progress_lines[y].add_glyph(code, fg_r, fg_g, fg_b, bg_r, bg_g, bg_b);
  }

  process_done() {
    BaseTerminal.prototype.process_done.apply(this, []);

    for (const line in this.in_progress_lines) {
      const replacement = this.in_progress_lines[line].spans;
      const lineElement = this.linesParentElement.children[line] as HTMLSpanElement;
      lineElement.innerHTML = '';
      lineElement.append(...replacement);
    }
    this.in_progress_lines = {};
  }
}

const terminalElement = document.getElementById('terminal') as HTMLPreElement;
if (terminalElement === null)
  throw Error("Couldn't find terminal element.");

const terminal = new DOMTerminal(terminalElement);
await terminal.init();

const exports = WebAssembly.Module.exports(terminalModule);
const imports = WebAssembly.Module.imports(terminalModule);
console.log(exports);
console.log(imports);

const protocol = location.protocol === 'https:' ? 'wss' : 'ws';

// Create WebSocket connection.
const socket = new WebSocket(`${protocol}://${window.location.host}/terminal`);

// Connection opened
socket.addEventListener('open', function (event) {});

let received = 0;
let nextToProcess = 0;
let backLog: {[key: number]: Uint8Array} = {}

let processDataTimout = 0;

function queueTerminalUpdate() {
  if (processDataTimout > 0)
    return;

  processDataTimout = window.setTimeout(function() {
    for (let i = nextToProcess; nextToProcess != received; ++i) {
      const data = backLog[i];
      delete backLog[i];

      if (data === undefined)
        break;

      terminal.process_data(data, false);
      nextToProcess = i + 1;
    }

    terminal.process_done();
    processDataTimout = 0;
  }, 15);
}

// Listen for messages
socket.addEventListener('message', async function (event) {
  const current = received++;

  // If multiple messages come in at the same time this await may resolve
  // out of order. We add the item to the backlog and then go through it
  // in order of what's not yet processed.

  const buffer = await event.data.arrayBuffer();
  const data = new Uint8Array(buffer);

  backLog[current] = data;

  queueTerminalUpdate();
});

socket.addEventListener('close', async function (event) {
  // Reset style, return home, clear screen, type 'Disconnected..'
  const msg = '\x1b[m;\x1b[H\x1b[2JDisconnected..';
  const encoder = new TextEncoder();
  const data = encoder.encode(msg);
  terminal.process_data(data);
  queueTerminalUpdate();
});

// Periodic ping
setInterval(() => {
  socket.send(new Uint8Array());
}, 1000 * 50)

function applyBracketedPaste(buffer: Uint8Array): Uint8Array {
  const bpAnnotationSize = 6;
  const resultBuffer = new Uint8Array(buffer.length + bpAnnotationSize * 2)

  // ESC [ 200 ~
  resultBuffer.set([27, 91, 32, 30, 30, 126], 0)

  let resultIndex = bpAnnotationSize;
  let inputIndex = 0;

  while (inputIndex != buffer.length) {
    if (buffer[inputIndex] >= 32) {
      resultBuffer[resultIndex++] = buffer[inputIndex];
    }

    ++inputIndex;
  }

  // ESC [ 201 ~
  resultBuffer.set([27, 91, 32, 30, 31, 126], resultIndex);
  resultIndex += bpAnnotationSize;

  return resultBuffer.slice(0, resultIndex)
}

async function pasteText(text: string) {
  const encoder = new TextEncoder();
  let encodedText = encoder.encode(text);

  if (terminal.bracketed_paste_mode())
    encodedText = applyBracketedPaste(encodedText);

  socket.send(encodedText);
}

async function pasteClipboard() {
  const clipboardContents = await navigator.clipboard.readText();
  await pasteText(clipboardContents);
}

document.getRootNode().addEventListener('paste', event => {
  if (!(event instanceof ClipboardEvent))
    throw new Error('Not a clipboard event')

  if (event.clipboardData === null)
    throw new Error('Missing clipboard data')

  pasteText(event.clipboardData.getData('text/plain'));
})

function isCharacterKeyPress(evt: KeyboardEvent) {
  return /^.$/u.test(evt.key);
}

function hasTextSelected(): boolean {
  const selection = window.getSelection()
  if (selection === null)
    return false;

  return selection.type === 'Range';
}

document.addEventListener('keydown', event => {
  let data = undefined;
  if (isCharacterKeyPress(event)) {

    if (event.ctrlKey && ['v', 'V'].includes(event.key)) {
      // Paste implemented in event handler. Make sure ^V doesn't type
      // any characters.
    } else if(event.ctrlKey && ['c', 'C'].includes(event.key) && hasTextSelected()) {
      // ^C is not forwarded to the terminal if text was selected, since
      // the user probably wants to copy text from the terminal.
    } else {
      const encoder = new TextEncoder();
      data = encoder.encode(event.key);
      if (event.ctrlKey && data.length === 1 && data[0] < 128)
        data[0] &= 31;

      if (event.altKey) {
        const newData = new Uint8Array(data.length);
        newData[0] = 27;
        newData.set(data, 1);
      }
    }
  } else {
    const conversion: {[key: string]: number[]} = {
      'Enter': [13],
      'Backspace': [127],
      'Tab': [9],
      'Escape': [27],
      'ArrowUp': [27, 91, 65],
      'ArrowDown': [27, 91, 66],
      'ArrowRight': [27, 91, 67],
      'ArrowLeft': [27, 91, 68],
      'PageUp': [27, 91, 53, 126],
      'PageDown': [27, 91, 54, 126],
      'Insert': [27, 91, 50, 126],
      'Delete': [27, 91, 51, 126],
    }
    const arrValue = conversion[event.key];
    if (arrValue) {
      data = new Uint8Array(arrValue)
    } else {
      console.log(`Unhandled special ${event.key}.`);
    }
  }

  if (data !== undefined) {
    event.preventDefault();
    socket.send(data);
  }
})

})()
