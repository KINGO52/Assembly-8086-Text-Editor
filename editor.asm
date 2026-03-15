; ==============================================================
; editor.asm  --  8086 Text Editor
; ==============================================================
; Ongoing work:
;   - Finding and patching bugs in the code
;   - Auto-save every AUTOSAVE_LIMIT edits to a temp file
; ==============================================================

JUMPS
IDEAL
MODEL small
STACK 100h
DATASEG
	; ---- Data-model constants -----------------------------------------------
	MAX_LINES     equ 200         ; Maximum number of lines held in the buffer
	MAX_LINELEN   equ 80          ; Maximum characters per line (columns 0-79)

	; ---- Core text buffer ---------------------------------------------------
	; Lines are stored contiguously.  Address of line i:
	;   offset lines + (i * MAX_LINELEN)
	lines         db MAX_LINES * MAX_LINELEN dup(0)

	; Actual character count for each line (word array, 0-based).
	line_lengths  dw MAX_LINES dup(0)

	; Number of lines currently loaded in the buffer.
	line_count    dw 0

	; ---- Editor state -------------------------------------------------------
	cur_line      dw 0            ; Cursor: current line index (0-based)
	cur_col       dw 0            ; Cursor: current column index (0-based)

	; Index of the buffer line shown on screen row 1 (top of viewport).
	scroll_offset dw 0

	; ---- File / save state --------------------------------------------------
	file_dirty    db 0            ; 1 = buffer has unsaved changes, 0 = clean
	edit_counter  db 0            ; Counts edits since the last autosave
	AUTOSAVE_LIMIT equ 10         ; Trigger autosave after this many edits

	; Null-terminated path for the temporary/backup file written on each save.
	temp_filename db 64 dup(0)

	; ---- UI strings ---------------------------------------------------------
	msg         db 'Enter filname (including file extension) $'
	filename    db 64             ; DOS buffered-input max-length byte
	            db ?              ; Actual length byte (filled by INT 21h / 0Ah)
	            db 64 dup (?)     ; Character buffer
	headername  db 65 dup (?)
	emptyname   db 'you entered nothing...$'
	erroropening db 'there was an error while opening the file did you perhaps enter the wrong name?$'
	filehandle  dw ?
	readres     db 4096 dup (?)   ; Raw file read buffer (max 4 096 bytes)
	lengthr     dw ?              ; Number of bytes returned by the last read
	header      db 'Alon`s File Editor - Current File: $'
	char_location dw ?            ; Video-memory offset used during rendering
	crlf_str    db 0Dh, 0Ah      ; CRLF pair written between lines on save

CODESEG

; --------------------------------------------------------------
; goto_pos row, col
; Sets the hardware cursor to (row, col) via BIOS INT 10h / 02h.
; Clobbers: AH, DH, DL  (caller must save if needed)
; --------------------------------------------------------------
macro goto_pos row, col
    mov ah, 02h
    mov dh, row
    mov dl, col
    int 10h
endm

; --------------------------------------------------------------
; movm2m dst, src
; Memory-to-memory move using AX as a temporary register.
; Expands to three instructions; AX is restored after the copy.
; --------------------------------------------------------------
macro movm2m op1, op2
	push ax
	mov ax, op2
	mov op1, ax
	pop ax
endm

; ==============================================================
; getFile
; Prompts the user for a filename and reads it into the DOS
; buffered-input structure at [filename] (INT 21h / 0Ah).
; After return, [filename+1] holds the character count and
; [filename+2] is the first character of the entered string.
; ==============================================================
proc getFile
	mov dx, offset msg      ; point to the prompt string
	mov ah, 9h
	int 21h

	mov dx, offset filename ; hand the buffered-input block to DOS
	mov bx, dx
	mov ah, 0Ah
	int 21h

	ret
endp

; ==============================================================
; checkfile
; Validates the filename entered by getFile and opens it for
; reading.  On success, [filehandle] receives the DOS file
; handle.  On failure the appropriate error message is printed.
;
; Registers modified: AX, BX, DX (all saved/restored internally)
; ==============================================================
proc checkfile
	mov al, [filename+1]    ; length byte written by INT 21h / 0Ah
	cmp al, 0
	je invalid_input        ; nothing was typed

	; Build a null-terminated string for DOS file-open (INT 21h / 3Dh).
	; The buffered-input layout is: [max][len][chars...], so
	; terminator goes at chars[len] == filename+2+len.
	lea bx, [filename]
	add bl, [bx+1]          ; advance BL past the max and len bytes
	mov [byte ptr bx+2], 0  ; write the null terminator

	mov dx, offset filename+2  ; DS:DX = null-terminated filename
	mov al, 0                  ; access mode: read-only
	mov ah, 3Dh
	int 21h
	jc erroropen               ; CF set means open failed

	mov bx, ax                 ; save the handle returned in AX
	mov [filehandle], ax
	jmp endcheckfile

	; ---- Error paths --------------------------------------------------------
	erroropen:
		mov dx, offset erroropening
		mov ah, 9h
		int 21h
		jmp endcheckfile
	invalid_input:
		mov dx, offset emptyname
		mov ah, 9h
		int 21h
	endcheckfile:
		ret
endp

; ==============================================================
; parse_readres_into_lines
;
; Parses raw file content from [readres] (length in [lengthr])
; into the lines[] buffer, populating line_lengths[] and
; line_count.  Handles CR, LF, and CRLF line endings.
;
; Register map (preserved across the call):
;   SI  = read pointer into readres (never aliased below)
;   BX  = current line index (0-based)
;   DI  = current column within the line (0-based)
;   CX  = bytes remaining to consume
;
; Design notes:
;   - BX is pushed/popped whenever it must temporarily hold a
;     pointer so that SI is never clobbered.
;   - Every byte consumed by lodsb is immediately followed by
;     dec cx so the count stays accurate.
;   - CRLF peek reads from the current SI position, not the
;     start of the buffer.
; ==============================================================
proc parse_readres_into_lines
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    xor bx, bx                    ; BX = line index (starts at 0)
    xor di, di                    ; DI = column offset within current line

    mov si, offset readres        ; SI = read pointer (never clobbered below)
    mov cx, [lengthr]             ; CX = bytes remaining

    or cx, cx                     ; ZF set if file is empty
    jz parse_done                 ; nothing to parse -- leave a zeroed buffer

parse_char:
    or cx, cx
    jz handle_eof                 ; all bytes consumed

    lodsb
    dec cx                        ; account for the byte lodsb just advanced SI over

    cmp al, 0
    je parse_char                 ; ignore embedded null bytes in the raw data

    cmp al, 0Dh                   ; CR (Carriage Return)
    je handle_cr

    cmp al, 0Ah                   ; LF (Line Feed)
    je handle_lf

    ; ---- Regular character --------------------------------------------------
    cmp di, MAX_LINELEN
    jae parse_char                ; line is full; discard the character silently

    ; Write AL into lines[BX * MAX_LINELEN + DI].
    ; BX is the line index; borrow it as a pointer, then restore it.
    push bx
    push ax                       ; preserve character while computing address
    mov ax, MAX_LINELEN
    mul bx                        ; AX = line_index * MAX_LINELEN
    add ax, di                    ; + column offset
    add ax, offset lines          ; + base of buffer
    mov bx, ax                    ; BX = final destination pointer
    pop ax                        ; restore character into AL
    mov [bx], al
    pop bx                        ; restore line index

    inc di
    jmp parse_char

handle_cr:
    ; Commit the current line length and advance to the next line.
    push bx
    mov ax, bx
    shl ax, 1                     ; word-array index (each entry is 2 bytes)
    add ax, offset line_lengths
    mov bx, ax
    mov [bx], di                  ; line_lengths[line_index] = column count
    pop bx

    inc bx
    cmp bx, MAX_LINES
    jae parse_done                ; buffer is full; stop
    xor di, di                    ; reset column for the new line

    ; Peek at the next byte to handle CRLF sequences.
    or cx, cx
    jz parse_done                 ; nothing left to peek at
    lodsb
    dec cx                        ; count the peeked byte
    cmp al, 0Ah                   ; is the next byte LF?
    je parse_char                 ; yes -- discard it and continue
    ; Not LF: un-consume the peeked byte and process it next iteration.
    dec si
    inc cx
    jmp parse_char

handle_lf:
    ; Stand-alone LF: same line-commit logic as CR.
    push bx
    mov ax, bx
    shl ax, 1
    add ax, offset line_lengths
    mov bx, ax
    mov [bx], di
    pop bx

    inc bx
    cmp bx, MAX_LINES
    jae parse_done
    xor di, di
    jmp parse_char

handle_eof:
parse_done:
    ; If the final line has content that was never terminated by a newline,
    ; store its length and include it in the count.
    cmp di, 0
    je store_count

    push bx
    mov ax, bx
    shl ax, 1
    add ax, offset line_lengths
    mov bx, ax
    mov [bx], di
    pop bx
    inc bx

store_count:
    mov [line_count], bx

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
endp

; ==============================================================
; mark_line_endings
;
; Writes a marker byte at position line_lengths[i] in every line
; (i.e. at the byte immediately after the last character).
;   AL = 0Ah  -> mark  (used before raw video output)
;   AL = 0    -> unmark (restores null terminator)
; ==============================================================
proc mark_line_endings
    push ax
    push bx
    push cx
    push dx

    mov dl, al                    ; preserve marker byte; AL will be clobbered
    xor cx, cx                    ; CX = line index

mark_loop:
    cmp cx, [line_count]
    jae mark_done

    ; BX = line_lengths[cx]
    mov bx, cx
    shl bx, 1                     ; word-array index
    mov bx, [line_lengths+bx]     ; BX = length of this line
    cmp bx, MAX_LINELEN
    jae skip_mark_write           ; length at or beyond column limit; skip

    ; Compute: &lines[cx * MAX_LINELEN + BX]
    push bx                       ; save length
    mov ax, MAX_LINELEN
    mul cx                        ; AX = cx * MAX_LINELEN
    pop bx
    add ax, bx                    ; + column offset (= length)
    add ax, offset lines          ; + buffer base

    mov bx, ax
    mov [bx], dl                  ; write the marker byte
    skip_mark_write:
    inc cx
    jmp mark_loop

mark_done:
    pop dx
    pop cx
    pop bx
    pop ax
    ret
endp

; ==============================================================
; readfile
;
; Reads up to 4 096 bytes from [filehandle] into [readres],
; then delegates parsing to parse_readres_into_lines.
; Afterwards, the screen is cleared and the header is rendered.
; Editor state (cur_line, cur_col, scroll_offset, dirty flag)
; is reset to its initial values.
; ==============================================================
proc readfile

    mov ah, 3Fh               ; DOS: read from file handle
    mov bx, [filehandle]
    mov cx, 4096              ; maximum bytes to read
    mov dx, offset readres
    int 21h
    mov [lengthr], ax         ; AX = bytes actually read

    ; Break the raw bytes into individual lines.
    call parse_readres_into_lines

    ; ---- Clear screen (mode 3 = 80x25 colour text) --------------------------
    mov ah, 0
    mov al, 3
    int 10h

    ; ---- Render the "editor name + filename" header on row 0 ----------------
    mov dx, offset header
    mov ah, 9h
    int 21h

    ; Temporarily terminate the filename with '$' so INT 21h / 09h can print it.
    lea bx, [filename]
    add bl, [bx+1]            ; BL now points past the length byte
    mov [byte ptr bx+2], '$'

    mov dx, offset filename
    add dx, 2                 ; skip max-len and actual-len bytes
    mov ah, 9h
    int 21h

    ; Restore the null terminator so the filename remains ASCIIZ.
    lea bx, [filename]
    add bl, [bx+1]
    mov [byte ptr bx+2], 0

    ; ---- Reset all editor state to start-of-file ----------------------------
    xor ax, ax
    mov [cur_line], ax
    mov [cur_col], ax
    mov [scroll_offset], ax
    mov [file_dirty], al
    mov [edit_counter], al

    ; Place the hardware cursor at row 1, column 0 (below the header).
    mov ah, 02h
    xor bh, bh
    mov dh, 1
    xor dl, dl
    int 10h

    ret
endp

; ==============================================================
; save_file
;
; Writes the entire lines[] buffer back to disk, one line at a
; time, separated by CRLF pairs.  No CRLF is written after the
; final line.  The file is created/truncated by INT 21h / 3Ch
; before writing begins.
; ==============================================================
proc save_file
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    ; Create (or truncate) the file so we always start with a clean slate.
    mov ah, 3Ch
    mov cx, 0                   ; normal attribute (no read-only, hidden, etc.)
    mov dx, offset filename+2
    int 21h
    jc save_done                ; carry set = create failed; abort gracefully
    mov [filehandle], ax

    xor cx, cx                  ; CX = current line index

save_loop:
    cmp cx, [line_count]
    jae save_done

    ; Load this line's character count into DX.
    mov bx, cx
    shl bx, 1
    mov dx, [line_lengths + bx]

    cmp dx, 0
    je write_newline            ; empty line: skip the write, still emit CRLF

    ; Compute the start address of the line in the buffer.
    mov ax, MAX_LINELEN
    push dx                     ; save length across the multiply
    mul cx                      ; AX = cx * MAX_LINELEN
    add ax, offset lines        ; AX = pointer to line data
    pop dx                      ; restore length into DX

    ; Write DX bytes starting at AX.
    push cx                     ; preserve line index across INT 21h
    mov cx, dx                  ; CX = byte count for the write call
    mov dx, ax                  ; DX = data pointer
    mov ah, 40h                 ; DOS: write to file handle
    mov bx, [filehandle]
    int 21h
    pop cx                      ; restore line index

write_newline:
    ; Omit the trailing newline on the very last line.
    mov ax, cx
    inc ax
    cmp ax, [line_count]
    jae next_line

    ; Write the two-byte CRLF sequence.
    mov ah, 40h
    mov bx, [filehandle]
    push cx                     ; preserve line index
    mov cx, 2
    mov dx, offset crlf_str
    int 21h
    pop cx                      ; restore line index

next_line:
    inc cx
    jmp save_loop

save_done:
    ; Close the file handle regardless of how we got here.
    mov ah, 3Eh
    mov bx, [filehandle]
    int 21h

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
endp


; ==============================================================
; render_screen
;
; Redraws the entire visible area of the editor:
;   1. Clears the display (BIOS mode-set).
;   2. Writes the header (editor title + filename) to row 0
;      directly via video memory (segment 0B800h).
;   3. Dumps MAX_LINELEN * 23 bytes of the lines[] buffer to
;      STDOUT so DOS renders them on rows 1-23.
;   4. Calls update_cursor to place the hardware cursor.
;
; Note: the raw-buffer dump in step 3 is a work-in-progress;
; it does not yet honour scroll_offset.
; ==============================================================
proc render_screen
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	; ---- Clear display ------------------------------------------------------
	mov ah, 0
	mov al, 3                 ; 80x25 colour text mode
	int 10h

	; ---- Set up video segment -----------------------------------------------
	mov ax, 0B800h
	mov es, ax
	xor di, di                ; start writing from video offset 0 (row 0, col 0)

	; ---- Render the header string on row 0 ----------------------------------
	mov si, offset header
	mov ah, 07h               ; attribute: white on black

header_loop:
	lodsb
	cmp al, '$'               ; '$' is the DOS string terminator
	je header_done
	mov [es:di], al           ; character byte
	mov [es:di+1], ah         ; attribute byte
	add di, 2                 ; each screen cell is 2 bytes
	jmp header_loop

header_done:
	; Append the actual filename to the header row.
	lea bx, [filename]
	add bl, [bx+1]            ; point past the length byte
	mov [byte ptr bx+2], '$'  ; temporarily terminate with '$'

	mov si, offset filename
	add si, 2                 ; skip the max-len and actual-len prefix bytes

header_name_loop:
	lodsb
	cmp al, '$'
	je header_name_done
	mov [es:di], al
	mov [es:di+1], ah
	add di, 2
	jmp header_name_loop

header_name_done:
	; Restore the ASCIIZ null terminator that was replaced above.
	lea bx, [filename]
	add bl, [bx+1]
	mov [byte ptr bx+2], 0

	; Emit a line-feed so subsequent DOS output starts on row 1.
	mov ah, 02h
	mov dl, 0Ah
	int 21h

	; ---- Dump lines buffer to screen (rows 1-23) ----------------------------
	; TODO: render only the lines visible through scroll_offset.
	mov ah, 40h
	mov bx, 1                 ; handle 1 = STDOUT
	mov cx, MAX_LINELEN * 23  ; bytes to write (23 visible content rows)
	mov dx, offset lines
	int 21h

	call update_cursor

	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp

; ==============================================================
; update_cursor
;
; Positions the hardware cursor to match the logical editor
; position: screen row = cur_line - scroll_offset + 1,
; screen column = cur_col (clamped to 79).
; ==============================================================
proc update_cursor
	push ax

	; Compute: screen_row = cur_line - scroll_offset + 1
	mov ax, [cur_line]
	sub ax, [scroll_offset]
	inc ax                    ; row 0 is the header; content starts at row 1

	; Clamp the column to the visible range (0-79).
	mov dx, [cur_col]
	cmp dl, 79
	jbe col_ok
	mov dl, 79
col_ok:
	mov dh, al                ; DH = screen row, DL = screen column

	mov ah, 02h               ; BIOS: set cursor position
	xor bh, bh                ; page 0
	int 10h

	pop ax
	ret
endp

; ==============================================================
; main_loop
;
; Entry point for the editor session:
;   1. Clears the screen and prompts for a filename.
;   2. Opens the file and reads its content into the buffer.
;   3. Renders the initial screen.
;   4. Enters the keyboard read-dispatch loop.
;
; The loop dispatches on the byte returned in AL (ASCII) or AH
; (extended scan code when AL == 0).
; ==============================================================
proc main_loop
    mov ah, 0                 ; BIOS: set video mode
    mov al, 3                 ; 80x25 colour text -- also clears the screen
    int 10h

    call getFile
    call checkfile

    ; Emit a newline so the file read starts below the prompt line.
    mov dl, 0Ah
    mov ah, 2h
    int 21h

    call readfile             ; reads, parses, resets state, draws header
    call render_screen        ; initial full-screen render

    ; Supervisor registers for row/column (kept here for legacy reasons;
    ; the real state lives in cur_line / cur_col).
    mov dh, 1                 ; row 1 = first content row
    mov dl, 0                 ; column 0

    ; ---- Keyboard read-dispatch loop ----------------------------------------
    read_key:
		mov ah, 00h           ; BIOS: wait for keypress
		int 16h               ; AL = ASCII code, AH = scan code

		; Dispatch on ASCII first.
		cmp al, 0
		je handle_extended    ; AL = 0 means this is an extended key
		cmp al, 08h           ; Backspace
		je handle_backspace
		cmp al, 0Dh           ; Enter / Return
		je handle_enter
		cmp al, 1Bh           ; Escape -- quit and save
		je quit
		cmp al, 13h           ; Ctrl+S -- also quit and save
		je quit

		; Any other printable character falls through to normal insertion.
		jmp normal

		; ---- Extended key dispatch ------------------------------------------
		handle_extended:
			; AH holds the BIOS scan code for non-ASCII keys.
			cmp ah, 48h
			je up_arrow
			cmp ah, 50h
			je down_arrow
			cmp ah, 4Bh
			je left_arrow
			cmp ah, 4Dh
			je right_arrow
			cmp ah, 47h       ; Home key
			je home
			cmp ah, 4Fh       ; End key
			je end_key
			jmp read_key      ; unknown extended key -- ignore

	; ---- Home key -----------------------------------------------------------
	home:
		mov [cur_col], 0      ; jump to column 0 on the current line
		call update_cursor
		jmp read_key

	; ---- End key ------------------------------------------------------------
	end_key:
		push bx
		mov bx, [cur_line]
		shl bx, 1
		add bx, offset line_lengths
		cmp [word ptr bx], 80 ; if line is completely full, cap at column 79
		jl normal_end
		mov [cur_col], 79
		call update_cursor
		jmp read_key
		normal_end:
			movm2m [cur_col], [bx]  ; set column to the actual line length
			pop bx
			call update_cursor
			jmp read_key

	; =========================================================================
	; handle_backspace
	;
	; Two cases:
	;   A. cur_col > 0: delete the character to the left of the cursor,
	;      shift the remainder of the line left by one, and redraw.
	;   B. cur_col == 0: merge the current line onto the end of the
	;      previous line (if their combined length fits in MAX_LINELEN).
	; =========================================================================
	handle_backspace:
		cmp [cur_col], 0
		je backspace_start_of_line

		dec [cur_col]             ; the cursor now points at the character to delete

		push ax
		push bx
		push cx
		push dx
		push si
		push di

		; ---- Compute how many characters lie to the right of the gap --------
		mov bx, [cur_line]
		shl bx, 1
		mov cx, [line_lengths + bx]  ; CX = total line length
		mov ax, [cur_col]
		sub cx, ax
		dec cx                    ; CX = chars to shift (everything after the gap)

		; Compute address of the deleted character slot in lines[].
		mov ax, [cur_line] 		; ax = current line number
		mov dx, MAX_LINELEN 	; dx = maximum line length
		mul dx 					; ax = current line number * maximum line length
		add ax, [cur_col] 		; ax = current line number * maximum line length + current column number
		add ax, offset lines 	; ax = current line number * maximum line length + current column number + offset of lines array
		mov di, ax 				; di = destination (the deleted slot)
		mov si, ax 				; si = source (first char after the gap) - 1
		inc si 					; si = source (first char after the gap)

		cmp cx, 0
		jle skip_bs_shift

		; Shift the tail of the line one byte to the left (DS:SI -> DS:DI).
		push cx
		push es
		mov ax, ds
		mov es, ax
		cld								; clear direction flag
		rep movsb						; sets ds:si to ds:di (usually es:di in this case it is the same as ds:di because es = ds) cx times with si and di increasing by one
		pop es
		pop cx

	skip_bs_shift:
		; Blank the vacated cell at the end of the line.
		; After rep movsb, DI already points to the correct position.
		mov [byte ptr di], ' '

		; Shrink the recorded line length by one.
		mov bx, [cur_line]
		shl bx, 1
		dec [word ptr line_lengths + bx]

		; ---- Redraw the affected portion of the line in video memory --------
		; Video offset of the cursor position: (cur_line) * 160 + 160 + cur_col * 2
		; (+160 skips the header row)
		mov ax, 0B800h
		mov es, ax

		mov ax, [cur_line]
		mov dx, 160
		mul dx
		add ax, 160               ; skip over the header row

		mov bx, [cur_col]
		shl bx, 1                 ; word offset within the row
		add ax, bx
		mov di, ax                ; DI = video destination
		mov si, ax
		add si, 2                 ; SI = one cell to the right of DI

		cmp cx, 0
		jle skip_vis_bs_shift

		push cx
	vis_bs_shift_loop:
		; Copy each char+attribute pair one cell to the left.
		mov al, [es:si]
		mov [es:di], al
		mov al, [es:si+1]
		mov [es:di+1], al
		add si, 2
		add di, 2
		loop vis_bs_shift_loop
		pop cx

	skip_vis_bs_shift:
		; Blank the rightmost cell that was left behind after the shift.
		mov [byte ptr es:di], ' '
		mov [byte ptr es:di+1], 07h  ; attribute: white on black

		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax

		call update_cursor
		jmp read_key

	; ---- Backspace at column 0: attempt to join with the line above ---------
	backspace_start_of_line:
		cmp [cur_line], 0
		je backspace_ignore       ; already at line 0, col 0 -- nothing to do

		; Load the length of the previous line into CX.
		mov bx, [cur_line]
		dec bx
		shl bx, 1
		mov ax, [line_lengths + bx]
		mov cx, ax                ; CX = prev line length

		; Load the length of the current line into AX.
		mov bx, [cur_line]
		shl bx, 1
		mov ax, [line_lengths + bx]

		; Reject the join if the combined length would exceed MAX_LINELEN.
		push ax
		add ax, cx
		cmp ax, MAX_LINELEN
		pop ax
		ja backspace_ignore

		; Position the cursor at the end of the previous line.
		mov [cur_col], cx

		; Copy the current line's characters onto the end of the previous line.
		xor dx,dx ;by default dx = 0 to prevent errors
		cmp ax, 0
		je join_empty_line        ; current line is empty -- only metadata to update

		push cx                   ; save previous line length
		push ax                   ; save current line length

		; Source: start of lines[cur_line]
		mov ax, [cur_line]
		mov dx, MAX_LINELEN
		mul dx
		add ax, offset lines
		mov si, ax

		; Destination: lines[cur_line - 1] + prev_len
		mov ax, [cur_line]
		dec ax
		mov dx, MAX_LINELEN
		mul dx
		pop dx                    ; restore current length into DX
		pop cx                    ; restore previous length into CX
		add ax, cx                ; offset past the existing chars on the prev line
		add ax, offset lines
		mov di, ax

		push cx
		push dx

		mov cx, dx                ; CX = number of bytes to copy
		mov ax, ds
		mov es, ax
		rep movsb

		pop dx
		pop cx

	join_empty_line:
		; Write the new combined length into line_lengths[cur_line - 1].
		mov bx, [cur_line]
		dec bx
		shl bx, 1
		mov ax, cx
		add ax, dx                ; prev_len + current_len
		mov [line_lengths + bx], ax

		; Shift all lines below cur_line one slot upward.
		mov ax, [cur_line]
		inc ax
		mov cx, [line_count]
		sub cx, ax
		jle update_line_count_bs  ; no lines exist below; skip the shift

		; Slide line_lengths[] entries up by one word.
		mov bx, [cur_line]
		shl bx, 1
		add bx, offset line_lengths
		mov di, bx
		mov si, bx
		add si, 2                 ; source is one entry ahead of destination

		push cx
	shift_lengths_up_loop:
		mov ax, [si]
		mov [di], ax
		add si, 2
		add di, 2
		loop shift_lengths_up_loop
		pop cx

		; Slide the text rows up by one slot (each slot is MAX_LINELEN bytes).
		mov ax, [cur_line]
		mov dx, MAX_LINELEN
		mul dx
		add ax, offset lines
		mov di, ax                ; destination = cur_line's slot
		mov si, ax
		add si, MAX_LINELEN       ; source = (cur_line + 1)'s slot

		mov ax, ds
		mov es, ax

	shift_text_up_loop:
		push cx
		push si
		push di

		mov cx, MAX_LINELEN / 2   ; move MAX_LINELEN bytes as words
		rep movsw

		pop di
		pop si
		pop cx
		add si, MAX_LINELEN       ; advance both pointers by one row
		add di, MAX_LINELEN
		loop shift_text_up_loop

	update_line_count_bs:
		dec [line_count]

		; Zero-fill the last slot that was vacated by the upward shift.
		mov ax, [line_count]
		mov dx, MAX_LINELEN
		mul dx
		add ax, offset lines
		mov di, ax
		mov cx, MAX_LINELEN / 2
		xor ax, ax
		rep stosw                 ; clear MAX_LINELEN bytes with zero words

		dec [cur_line]

		; Scroll the viewport up if the new cur_line is above the top row.
		mov ax, [scroll_offset]
		cmp [cur_line], ax
		jae redraw_screen_bs
		mov ax, [cur_line]
		mov [scroll_offset], ax

	redraw_screen_bs:
		call render_screen
		call update_cursor
		jmp read_key

	backspace_ignore:
		jmp read_key

	; =========================================================================
	; handle_enter
	;
	; Inserts a newline at the cursor:
	;   1. Shifts all lines below cur_line one slot down.
	;   2. Splits the current line at cur_col: characters from
	;      cur_col onward move to the new line (cur_line + 1).
	;   3. Updates line_lengths[] for both halves.
	;   4. Updates cur_line and cur_col, scrolls if needed.
	;
	; Does nothing if line_count has already reached MAX_LINES.
	; =========================================================================
	handle_enter:
		mov ax, [line_count]
		cmp ax, MAX_LINES
		jae enter_done            ; buffer is full; silently ignore the keypress

		; CX = number of lines that need to shift down (lines after cur_line).
		mov cx, [line_count]
		sub cx, [cur_line]
		dec cx
		jl split_current_line_only
		jz split_current_line_only

		; ---- Shift line_lengths[] down by one --------------------------------
		; Iterate from the last entry backwards so we don't overwrite source data.
		mov bx, [line_count]
		dec bx
		shl bx, 1
		add bx, offset line_lengths

		push cx
	shift_lengths_loop:
		mov ax, [bx]
		mov [bx + 2], ax          ; copy entry[i] to entry[i+1]
		sub bx, 2
		loop shift_lengths_loop
		pop cx

		; ---- Shift text lines[] down by one ---------------------------------
		mov ax, [line_count]
		dec ax
		mov dx, MAX_LINELEN
		mul dx
		add ax, offset lines
		mov si, ax                ; source = last current line
		mov di, ax
		add di, MAX_LINELEN       ; destination = one slot below

		mov ax, ds
		mov es, ax                ; ensure ES = DS for rep movsw

	shift_text_lines_loop:
		push cx
		push si
		push di

		mov cx, MAX_LINELEN / 2
		rep movsw                 ; copy one full line (word-sized moves)

		pop di
		pop si
		pop cx
		sub si, MAX_LINELEN       ; walk both pointers one row upward
		sub di, MAX_LINELEN
		loop shift_text_lines_loop

	split_current_line_only:
		mov ax, ds
		mov es, ax

		; Zero-fill the new line slot (cur_line + 1) before copying into it.
		mov ax, [cur_line]
		inc ax
		mov dx, MAX_LINELEN
		mul dx
		add ax, offset lines
		mov di, ax
		mov cx, MAX_LINELEN / 2
		xor ax, ax
		rep stosw

		; Compute the number of characters that belong on the new line:
		;   new_line_len = line_lengths[cur_line] - cur_col
		mov bx, [cur_line]
		shl bx, 1
		mov cx, [line_lengths + bx]
		sub cx, [cur_col]

		; Guard: if cur_col was beyond the line length, treat new_line_len as 0.
		mov ax, cx
		cmp ax, 0
		jge set_new_length
		xor ax, ax
		mov cx, 0
	set_new_length:
		; Record the length of the new line (cur_line + 1).
		mov bx, [cur_line]
		inc bx
		shl bx, 1
		mov [line_lengths + bx], ax

		; Truncate the current line to cur_col characters.
		mov ax, [cur_col]
		mov bx, [cur_line]
		shl bx, 1
		mov [line_lengths + bx], ax

		cmp cx, 0
		jle finalize_enter        ; nothing to copy to the new line

		; ---- Copy tail characters to the new line ---------------------------
		; Source: lines[cur_line * MAX_LINELEN + cur_col]
		mov ax, [cur_line]
		mov dx, MAX_LINELEN
		mul dx
		add ax, [cur_col]
		add ax, offset lines
		mov si, ax

		; Destination: lines[(cur_line + 1) * MAX_LINELEN]
		mov ax, [cur_line]
		inc ax
		mov dx, MAX_LINELEN
		mul dx
		add ax, offset lines
		mov di, ax

	copy_split_loop:
		mov al, [si]
		mov [di], al
		mov [byte ptr si], 0      ; clear the character from the old line's tail
		inc si
		inc di
		loop copy_split_loop

	finalize_enter:
		inc [line_count]
		inc [cur_line]
		mov [cur_col], 0          ; cursor goes to the start of the new line

		; Scroll down one row if the new cur_line is below the visible area.
		mov ax, [scroll_offset]
		add ax, 23
		cmp [cur_line], ax
		ja scroll_down_ent
		call render_screen
		jmp read_key_enter_done

	scroll_down_ent:
		inc [scroll_offset]
		call render_screen
	read_key_enter_done:
		call update_cursor
		jmp read_key

	enter_done:
		jmp read_key              ; MAX_LINES reached -- ignore the keypress

	; =========================================================================
	; normal  (printable character insertion)
	;
	; Inserts the character in AL at the current cursor position:
	;   1. Shifts all characters from cur_col to end-of-line one
	;      byte to the right to make room.
	;   2. Writes the new character at cur_col.
	;   3. Redraws everything from cur_col to the new end-of-line
	;      in video memory.
	;   4. Advances cur_col by one.
	;
	; Silently ignores the keypress if the line is already full
	; (line_length == MAX_LINELEN).
	; =========================================================================
	normal:
		push cx
		push bx
		push ax
		push si
		push di

		mov bx, [cur_line]
		shl bx, 1
		mov cx, [line_lengths+bx]  ; CX = current (old) line length

		; Reject input if the line is already at its maximum capacity.
		cmp cx, MAX_LINELEN
		jae normal_done_full

		inc [word ptr line_lengths+bx] ; the line will grow by one character

		; Compute the address of the byte one past the current end of the line.
		mov ax, [cur_line]
		mov dx, MAX_LINELEN
		mul dx
		add ax, cx                 ; + old length (= offset of the last char + 1)
		add ax, offset lines
		mov di, ax                 ; DI = target for the rightmost character
		mov si, ax
		dec si                     ; SI = last actual character in the line

		; DX = number of characters that need to shift rightward.
		mov dx, cx
		sub dx, [cur_col]

		cmp dx, 0
		jle skip_func_shift        ; cursor is at or past end of line; no shift needed

		push cx                    ; save old line length across the shift
		mov cx, dx                 ; CX = character count to shift

		; Shift characters right using backwards rep movsb (direction flag = 1).
		push es
		mov ax, ds
		mov es, ax
		std                        ; set direction flag for backwards copy
		rep movsb
		cld                        ; restore direction flag to forward
		pop es
		pop cx

	skip_func_shift:
		; Write the new character at lines[cur_line * MAX_LINELEN + cur_col].
		mov ax, [cur_line]
		mov dx, MAX_LINELEN
		mul dx
		add ax, [cur_col]
		add ax, offset lines
		mov bx, ax

		; Retrieve the saved AL from the stack.
		; Stack layout at this point (bottom to top): cx, bx, ax, si, di
		; AL is the low byte of the saved AX, located 4 bytes above SP.
		mov bp, sp
		mov al, [bp+4]
		mov [byte ptr bx], al

		; ---- Redraw from cur_col to the new end of line in video memory -----
		; Video address: (cur_line * 160 + 160) + cur_col * 2
		mov ax, 0B800h
		mov es, ax

		mov ax, [cur_line]
		mov dx, 160
		mul dx
		add ax, 160               ; skip the header row
		mov bx, [cur_col]
		shl bx, 1
		add ax, bx
		mov di, ax                ; DI = video destination

		mov ax, [cur_line]
		mov dx, MAX_LINELEN
		mul dx
		add ax, [cur_col]
		add ax, offset lines
		mov si, ax                ; SI = lines[] source

		; CX = new line length - cur_col = number of characters to redraw.
		mov bx, [cur_line]
		shl bx, 1
		mov cx, [line_lengths+bx]
		sub cx, [cur_col]

	redraw_vis_loop:
		mov al, [si]
		mov [es:di], al
		mov [byte ptr es:di+1], 07h  ; attribute: white on black
		inc si
		add di, 2
		loop redraw_vis_loop

	normal_done:
		inc [cur_col]
		pop di
		pop si
		pop ax
		pop bx
		pop cx
		jmp update_cursor_pos

	normal_done_full:
		; Line full -- discard the keystroke and return to the read loop.
		pop di
		pop si
		pop ax
		pop bx
		pop cx
		jmp read_key

	; ---- Duplicate scan-code dispatch (kept for legacy compatibility) --------
	special_keys:
		cmp ah, 48h
		je up_arrow
		cmp ah, 50h
		je down_arrow
		cmp ah, 4Bh
		je left_arrow
		cmp ah, 4Dh
		je right_arrow
		cmp al, 13h
		je quit
		jmp read_key

	; =========================================================================
	; up_arrow
	;
	; Moves cur_line up by one.  If cur_col exceeds the length of the
	; target line, it is snapped back to that line's end.  Scrolls the
	; viewport up by one row if necessary.
	; =========================================================================
    up_arrow:
    ; Guard: do nothing if we are already on the first line.
    cmp [cur_line], 0
    je read_key

    dec [cur_line]

    ; If cur_col is past the end of the new line, snap it to line end.
    mov bx, [cur_line]
    shl bx, 1                   ; word-array index
    mov ax, [line_lengths+bx]
    cmp [cur_col], ax
    jbe skip_col_upd_up
    mov [cur_col], ax

	skip_col_upd_up:
		; Scroll the viewport up if cur_line has moved above the top edge.
		mov ax, [scroll_offset]
		cmp [cur_line], ax
		jb scroll_up

    call update_cursor
    jmp read_key

	scroll_up:
		dec [scroll_offset]
		call render_screen
		jmp read_key

	; =========================================================================
	; down_arrow
	;
	; Moves cur_line down by one.  Snaps cur_col if needed.
	; Scrolls the viewport down if the cursor moves past the bottom edge.
	; =========================================================================
    down_arrow:
    ; Guard: do nothing if we are already on the last line.
    mov ax, [line_count]
    dec ax
    cmp [cur_line], ax
    je read_key

    inc [cur_line]

    ; Snap cur_col if it exceeds the new line's length.
    mov bx, [cur_line]
    shl bx, 1
    mov ax, [line_lengths+bx]
    cmp [cur_col], ax
    jbe skip_col_upd
    mov [cur_col], ax

	skip_col_upd:
		; Scroll the viewport down if cur_line is now below the bottom edge.
		; The viewport shows 23 content rows (rows 1-23).
		mov ax, [scroll_offset]
		add ax, 23
		cmp [cur_line], ax
		ja scroll_down

    call update_cursor
    jmp read_key

	scroll_down:
		inc [scroll_offset]
		call render_screen
		jmp read_key

	; =========================================================================
	; left_arrow
	;
	; Moves the cursor one character to the left.
	; If already at column 0, wraps to the end of the previous line.
	; Does nothing if at line 0, column 0.
	; =========================================================================
    left_arrow:
        cmp [cur_col], 0
        ja notup                  ; column > 0; simple move left

		; Column is 0 -- try to wrap to the previous line.
		cmp [cur_line], 0
		je read_key               ; already at the very first position; ignore

		; Move up and set the column to the end of the previous line.
		dec [cur_line]
		mov bx, offset line_lengths
		push [cur_line]
		shl [cur_line], 1         ; word-array index
		add bx, [cur_line]
		movm2m [cur_col], [bx]    ; cur_col = line_lengths[cur_line]
		pop [cur_line]            ; restore the non-doubled value
		jmp update_cursor_pos

		; Simple left move (no wrap needed).
    notup:
        dec [cur_col]
		jmp update_cursor_pos

	; =========================================================================
	; right_arrow
	;
	; Moves the cursor one character to the right.
	; If already at the end of the line, wraps to the start of the next line.
	; Does nothing if at the last line's end.
	; =========================================================================
    right_arrow:
        ; Load the current line's length.
        mov bx, [cur_line]
        shl bx, 1
        mov ax, [line_lengths + bx]

        ; If cur_col < line_length, just move right within the same line.
        cmp [cur_col], ax
        jb move_right_same_line

        ; At the end of the line -- try to wrap to the start of the next line.
        mov ax, [line_count]
        dec ax
        cmp [cur_line], ax
        jae read_key             ; already on the last line; nowhere to go

        inc [cur_line]
        mov [cur_col], 0
        jmp update_cursor_pos

    move_right_same_line:
        inc [cur_col]
        jmp update_cursor_pos

; =========================================================================
; update_cursor_pos  (shared tail for arrow key handlers)
;
; Recalculates scroll_offset if cur_line has moved outside the viewport,
; clamps cur_col to the current line's length, then calls update_cursor.
; =========================================================================
update_cursor_pos:
		; Is cur_line above the viewport?
		mov ax, [cur_line]
		cmp ax, [scroll_offset]
		jb cursor_left_of_viewport

		; Is cur_line below the viewport?
		mov ax, [scroll_offset]
		add ax, 23
		cmp [cur_line], ax
		ja cursor_right_of_viewport

		; cur_line is within the viewport -- clamp cur_col and update.
		mov ax, [cur_line]
        shl ax, 1                 ; word-array index
        mov bx, ax
        mov ax, [line_lengths+bx]
		cmp [cur_col], ax
		jna upd
		mov [cur_col], ax         ; snap column to line end
	upd:
		call update_cursor
		jmp read_key

cursor_left_of_viewport:
		; cur_line scrolled above the top; reset scroll_offset to 0.
		mov [scroll_offset], 0
		call render_screen
		jmp read_key

cursor_right_of_viewport:
		; cur_line scrolled below the bottom; recompute scroll_offset.
		mov ax, [cur_line]
		sub ax, 23
		mov [scroll_offset], ax
		call render_screen
		jmp read_key

	; =========================================================================
	; quit
	;
	; Saves the buffer to disk, clears the screen, and terminates
	; the program via DOS INT 21h / 4Ch.
	; =========================================================================
	quit:
		call save_file            ; flush buffer to disk before exit

		; Clear the display.
		mov ah, 0
		mov al, 3
		int 10h

		; Home the cursor to (0, 0).
		mov dh, 0
		mov dl, 0
		goto_pos dh, dl

		; Clean program exit.
		mov ax, 4C00h
		int 21h

    ret
endp

; ==============================================================
; Program entry point
; ==============================================================
start:
	mov ax, @data
	mov ds, ax

	call main_loop

exit:
	mov ax, 4c00h
	int 21h
END start
