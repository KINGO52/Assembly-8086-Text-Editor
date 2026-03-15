# Assembly-8086-Text-Editor

A text editor written in 8086 Assembly (TASM) targeting real-mode DOS. It operates directly on video memory (`0B800h`) and uses DOS `INT 21h` system calls for file I/O.

---

## Features

- **File Loading** — Opens any text file by name at startup using DOS `INT 21h` (AH=3Dh), reads up to 4096 bytes, and parses CR, LF, and CRLF line endings into a structured line buffer.
- **File Saving** — On quit, truncates and rewrites the file using exact character counts from the `line_lengths` array, inserting CRLF only between lines (not after the last).
- **Insert Mode Typing** — Characters are inserted at the cursor position; trailing characters on the line shift right using a backwards `rep movsb`.
- **Backspace** — Mid-line backspace shifts trailing characters left using `rep movsb`. Backspacing at column 0 joins the current line to the end of the previous one, provided the combined length doesn't exceed `MAX_LINELEN`.
- **Enter / Line Splitting** — Splits the active line at the cursor, moving trailing characters to a new line below it and shifting all subsequent lines down.
- **Arrow Key Navigation** — Full support for `↑`, `↓`, `←`, `→` with column clamping when moving between lines of different lengths. Left/right arrow wraps across line boundaries.
- **Home / End Keys** — `Home` snaps to column 0; `End` snaps to the last character of the current line (clamped to column 79 max).
- **Viewport Scrolling** — Displays 23 lines of content at a time. The viewport scrolls automatically when the cursor moves above or below the visible area.
- **Header Row** — Row 0 permanently shows `Alon's File Editor - Current File: <filename>`.

---
##Limits
- Files With more Than 80 chars per line will have chars that are out of bounds not load and be deleted on saving

---

## Controls

| Key         | Action                                       |
|-------------|----------------------------------------------|
| Any char    | Insert character at cursor (insert mode)     |
| `Backspace` | Delete character to the left / join lines    |
| `Enter`     | Split line at cursor                         |
| `↑` / `↓`  | Move cursor up / down one line               |
| `←` / `→`  | Move cursor left / right (wraps lines)       |
| `Home`      | Move to start of line                        |
| `End`       | Move to end of line                          |
| `Esc`       | Save and quit                                |

---

## Data Model

The editor uses a flat static buffer layout:

```
lines         db MAX_LINES * MAX_LINELEN dup(0)   ; 200 × 80 byte grid
line_lengths  dw MAX_LINES dup(0)                 ; character count per line
line_count    dw 0                                ; number of active lines
cur_line      dw 0                                ; cursor row (0-based)
cur_col       dw 0                                ; cursor column (0-based)
scroll_offset dw 0                                ; topmost visible buffer line
```

- `MAX_LINES = 200`, `MAX_LINELEN = 80`
- Address of line `i`: `offset lines + (i × 80)`
- Length of line `i`: `line_lengths[i * 2]` (word array)

---

## Building

Requires **TASM** and **TLINK** (Turbo Assembler).

```bat
TASM editor.asm
TLINK editor.obj
```
If you want there is a relases page with pre-built versions https://github.com/KINGO52/Assembly-8086-Text-Editor/releases

Run `EDITOR.EXE` in a DOS environment (DOSBox recommended).
---

## Project Structure

| File         | Description                              |
|--------------|------------------------------------------|
| `editor.asm` | Full source — data, macros, and all procs |
| `README.md`  | This file                                |


* This is currently the same as patch 4 for previous (not fully functioning versions) check other trees
