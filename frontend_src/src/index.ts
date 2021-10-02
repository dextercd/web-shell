import { DOMTerminal } from './dom_terminal'

(async () => {

const terminalElement = document.getElementById('terminal') as HTMLPreElement;
if (terminalElement === null)
  throw Error("Couldn't find terminal element.");

const terminal = new DOMTerminal(terminalElement);
await terminal.init();

const protocol = location.protocol === 'https:' ? 'wss' : 'ws';

// Create WebSocket connection.
const socket = new WebSocket(`${protocol}://${window.location.host}/terminal`);

function recalculateTerminalSize() {
  const [width, height] = terminal.getIdealSize()
  console.log(`Resize request to ${width} by ${height}.`)
  sendResize(width, height)
}

const resizeObserver = new ResizeObserver(() => {
  recalculateTerminalSize()
})

// Connection opened
socket.addEventListener('open', function (event) {
  recalculateTerminalSize()
  resizeObserver.observe(terminalElement);
});

enum SpecialEntry {
  InProgress,
  Nothing
}

type LogEntry = Uint8Array | SpecialEntry;

let backLog: {[key: number]: LogEntry} = {}

let processDataTimout = 0;
let received = 0;
let nextToProcess = 0;

function queueTerminalUpdate() {
  if (processDataTimout > 0)
    return;

  processDataTimout = window.setTimeout(function() {
    for (let i = nextToProcess; nextToProcess != received; ++i) {
      const data = backLog[i];

      if (data === undefined || data === SpecialEntry.InProgress)
        break;

      // Delete entry unless it's an in-progress entry
      delete backLog[i];

      if (data === SpecialEntry.Nothing) {
        // Nothing to do
      } else if (data instanceof Uint8Array) {
        terminal.process_data(data, false);
      }

      nextToProcess = i + 1;
    }

    terminal.process_done();
    processDataTimout = 0;
  }, 5);
}

function handleOutput(current: number, data: Uint8Array) {
  backLog[current] = data;
  queueTerminalUpdate();
}

function handleResize(width: number, height: number) {
  terminal.resize(width, height);
  terminal.process_done();
  console.log(`Resized to ${width} by ${height}.`)
}

const responseOutput = 111; // o
const responseResize = 114; // r

// Listen for messages
socket.addEventListener('message', async function (event) {
  const current = received++;

  // If multiple messages come in at the same time this await may resolve out
  // of order. We reserve an item in the backlog and then reassign it or mark
  // it as 'nothing' after we know what message we got.
  backLog[current] = SpecialEntry.InProgress;

  const buffer = await event.data.arrayBuffer();
  const view = new DataView(buffer);

  switch (view.getUint8(0)) {
    case responseOutput: {
      handleOutput(current, new Uint8Array(buffer, 1));
      break;
    }

    case responseResize: {
      const width = view.getUint32(1)
      const height = view.getUint32(5)
      handleResize(width, height);
      break;
    }
  }

  // If we didn't use the entry, mark it as something that can be ignored
  if (backLog[current] === SpecialEntry.InProgress) {
    backLog[current] = SpecialEntry.Nothing;
  }
});

socket.addEventListener('close', async function (event) {
  // Reset style, return home, clear screen, type 'Disconnected..'
  const msg = '\x1b[m;\x1b[H\x1b[2JDisconnected..';
  const encoder = new TextEncoder();
  const data = encoder.encode(msg);
  terminal.process_data(data);
  queueTerminalUpdate();
});

const socketWriteInput = 119; // w
const socketResizeTerminal = 114; // r

function sendInput(data: Uint8Array) {
  const buffer = new Uint8Array(data.length + 1);
  buffer.set([socketWriteInput], 0);
  buffer.set(data, 1);
  socket.send(buffer);
}

function sendResize(width: number, height: number) {
  const buffer = new ArrayBuffer(1 + 2 * 4);
  const view = new DataView(buffer);
  view.setUint8(0, socketResizeTerminal);
  view.setUint32(1, width);
  view.setUint32(5, height);
  socket.send(buffer);
}

// Periodic ping
setInterval(() => {
  socket.send(new Uint8Array());
}, 1000 * 50)

function applyBracketedPaste(buffer: Uint8Array): Uint8Array {
  const bpAnnotationSize = 6;
  const resultBuffer = new Uint8Array(buffer.length + bpAnnotationSize * 2)

  // ESC [ 200 ~
  resultBuffer.set([27, 91, 50, 48, 48, 126], 0)

  let resultIndex = bpAnnotationSize;
  let inputIndex = 0;

  while (inputIndex != buffer.length) {
    const byte = buffer[inputIndex++];
    if ((byte >= 32) ||
        (byte >= 9 && byte <= 13))
    {
      resultBuffer[resultIndex++] = byte;
    }
  }

  // ESC [ 201 ~
  resultBuffer.set([27, 91, 50, 48, 49, 126], resultIndex);
  resultIndex += bpAnnotationSize;

  return resultBuffer.slice(0, resultIndex)
}

async function pasteText(text: string) {
  text.replaceAll('\n', '\r')
  const encoder = new TextEncoder();
  let encodedText = encoder.encode(text);

  if (terminal.bracketed_paste_mode())
    encodedText = applyBracketedPaste(encodedText);

  sendInput(encodedText);
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

terminalElement.addEventListener('keydown', event => {
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
    sendInput(data);
  }
})

// I sometimes accidentally use a hotkey that closes the tab, such as C-w in Vim
// to manipulate windows. Make sure the user actually wants to leave the site.
window.addEventListener('beforeunload', function (e) {
  e.preventDefault();
  e.returnValue =
    'Are you sure you want to close the terminal? ' +
    'Unsaved changes will be lost.';
});

})()
