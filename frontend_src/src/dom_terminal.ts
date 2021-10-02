import { BaseTerminal } from './base_terminal'

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

export class DOMTerminal extends BaseTerminal {
  width = 80;
  height = 24;
  cursorx = 0;
  cursory = 0;
  root_element: HTMLPreElement;
  base_element: HTMLDivElement;
  linesParentElement: HTMLDivElement;
  cursor_element: HTMLSpanElement;
  in_progress_lines: {[key: number]: LineInProgress} = {};
  enableCursorSmoothMove: boolean;

  constructor(root_element: HTMLPreElement) {
    super();

    // Fixed element translate animation is not working properly
    // on Chrome.  Disable for now.
    this.enableCursorSmoothMove = !navigator.userAgent.includes('Chrome');

    this.root_element = root_element;
    this.root_element.innerHTML = '';

    this.base_element = document.createElement('div');
    this.base_element.id = 'base'
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

    // Need to have a size assigned to make sure that the size is determined by
    // CSS `resize` functionality.  (Not affected by other changes like resizing
    // the window.)
    this.base_element.style.height = `${this.base_element.offsetHeight}px`;
  }

  getIdealSize() {
    const rootWidth = this.base_element.offsetWidth;
    const rootHeight = this.base_element.offsetHeight;

    const charWidth = this.cursor_element.offsetWidth;
    const charHeight = this.cursor_element.offsetHeight;
    return [
      Math.floor(rootWidth / charWidth),
      Math.floor(rootHeight / charHeight)
    ]
  }

  set_cursor(x: number, y: number) {
    if (this.enableCursorSmoothMove) {
      const smoothMoveClass = 'cursor-smooth-move';

      // Smooth cursor movement is enabled depending on the distance of the
      // current and new postition.  A larger distance is allowed if the new
      // position is on the same axis.
      const distance = Math.abs(this.cursory - y) + Math.abs(this.cursorx - x);
      if (distance <= 8 && (this.cursory === y || this.cursorx === x) ||
          distance <= 4)
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
      glyphSpan.style.backgroundColor = 'black';

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
