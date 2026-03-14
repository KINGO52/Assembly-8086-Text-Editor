; current goals or problems
;adding editing functionality
;saving the buffer into the file as well as saving into a temp file on every edit


JUMPS
IDEAL
MODEL small
STACK 100h
DATASEG
	; --- DATA MODEL CONSTANTS ---
	MAX_LINES     equ 200         ; Max number of lines in the buffer
	MAX_LINELEN   equ 80          ; Max characters per line


	; --- CORE BUFFER ---
	; Raw storage for all lines, laid out contiguously.
	; Address of line 'i' = offset lines + (i * MAX_LINELEN)
	lines         db MAX_LINES * MAX_LINELEN dup(0)

	; Array to store the actual length of each line.
	line_lengths  dw MAX_LINES dup(0)

	; Current number of lines loaded in the buffer.
	line_count    dw 0

	; --- EDITOR STATE ---
	; Cursor position within the buffer.
	cur_line      dw 0            ; Current line index (0-based)
	cur_col       dw 0            ; Current column index (0-based)

	; Viewport state.
	scroll_offset dw 0            ; Buffer line index displayed at the top of the screen (row 1).

	; File state.
	file_dirty    db 0            ; Flag (1 if modified, 0 otherwise)
	edit_counter  db 0            ; Counter for autosave feature
	AUTOSAVE_LIMIT equ 10         ; Number of edits before triggering an autosave

	; Filename for temporary/backup file on save.
	temp_filename db 64 dup(0)

	msg db 'Enter filname (including file extension) $'
	filename db 64
			db ?
			db 64 dup (?)
	headername db 65 dup (?)
	emptyname db 'you entered nothing...$'
	erroropening db 'there was an error while opening the file did you perhaps enter the wrong name?$'
	filehandle dw ?
	readres db 4096 dup (?)
	lengthr dw ?
	header db 'Alon`s File Editor - Current File: $'
	char_location dw ? ; location to use for video memory
	crlf_str db 0Dh, 0Ah
CODESEG	
macro goto_pos row, col
    mov ah, 02h
    mov dh, row
    mov dl, col
    int 10h
endm
macro movm2m op1, op2 ; mov command compatible with memory to memory
	push ax
	mov ax, op2
	mov op1, ax
	pop ax
endm
proc getFile
	mov dx, offset msg ;user prompt
	mov ah, 9h
	int 21h
	
	mov dx, offset filename ; user input for file
	mov bx, dx
	mov ah, 0Ah
	int 21h
	
	ret
endp
proc checkfile
	mov al, [filename+1]
	cmp al, 0
	je invalid_input
	
	; make the file name 0 terminated for I/O
	lea bx, [filename]
	add bl, [bx+1]        ; length
	mov [byte ptr bx+2], 0
	
	
	mov dx, offset filename+2  ; pointer to filename
	mov al, 0                 ; access mode
	mov ah, 3Dh
	int 21h
	jc erroropen            ; CF=1 → error

	mov bx, ax                ; AX = file handle (save it!)
	mov [filehandle], ax
	jmp endcheckfile
	
	; -- error msg printing --
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

; ============================================================
; parse_readres_into_lines
; Parses raw file content from readres into the lines buffer.
; Handles CR, LF, and CRLF line endings.
; Updates line_count with the number of lines parsed.
; ============================================================
; ============================================================
; parse_readres_into_lines
; Fixes:
;   - SI no longer clobbered during line/length writes (BX used as ptr)
;   - CX properly decremented on every byte read
;   - CRLF peek reads from current SI, not start of buffer
;   - Dead handle_line_wrap removed
;   - Spurious mul in line_lengths indexing removed
; ============================================================
proc parse_readres_into_lines
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    xor bx, bx                    ; BX = line index (0-based)
    xor di, di                    ; DI = column offset within current line

    mov si, offset readres        ; SI = read pointer (never clobbered below)
    mov cx, [lengthr]             ; CX = bytes remaining (decremented every lodsb)

    or cx, cx
    jz parse_done

parse_char:
    or cx, cx
    jz handle_eof                 ; stop when all bytes consumed

    lodsb
    dec cx                        ; FIX: count down every byte we consume

    cmp al, 0
    je parse_char                 ; skip null bytes

    cmp al, 0Dh                   ; CR
    je handle_cr

    cmp al, 0Ah                   ; LF
    je handle_lf

    ; --- Regular character ---
    cmp di, MAX_LINELEN
    jae parse_char                ; line full, discard character

    ; Store AL into lines[BX * MAX_LINELEN + DI]
    ; BX is our line index - we need it as a ptr temporarily so push/pop it
    push bx
    push ax                       ; save character (in AL)
    mov ax, MAX_LINELEN
    mul bx                        ; AX = line_index * MAX_LINELEN
    add ax, di
    add ax, offset lines
    mov bx, ax                    ; BX = destination pointer
    pop ax                        ; restore character into AL
    mov [bx], al
    pop bx                        ; restore line index

    inc di
    jmp parse_char

handle_cr:
    ; Store line_lengths[BX] = DI
    push bx
    mov ax, bx
    shl ax, 1                     ; AX = BX * 2 (word array index)
    add ax, offset line_lengths
    mov bx, ax
    mov [bx], di                  ; FIX: uses BX as ptr, SI untouched
    pop bx

    inc bx
    cmp bx, MAX_LINES
    jae parse_done
    xor di, di

    ; Peek at next byte to handle CRLF
    or cx, cx
    jz parse_done                 ; nothing left to peek at
    lodsb
    dec cx                        ; FIX: count the peeked byte
    cmp al, 0Ah                   ; is it LF?
    je parse_char                 ; yes - consume it and move on
    ; Not LF - put byte back (un-consume)
    dec si
    inc cx
    jmp parse_char

handle_lf:
    ; Store line_lengths[BX] = DI
    push bx
    mov ax, bx
    shl ax, 1
    add ax, offset line_lengths
    mov bx, ax
    mov [bx], di                  ; FIX: uses BX as ptr, SI untouched
    pop bx

    inc bx
    cmp bx, MAX_LINES
    jae parse_done
    xor di, di
    jmp parse_char

handle_eof:
parse_done:
    ; If final line has content, store its length and count it
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
; ============================================================
; mark_line_endings
; Writes 'marker' byte at position line_lengths[i] in each line,
; i.e. the first null byte after each line's content.
; Call with AL = 0Ah to mark, AL = 0 to unmark.
; ============================================================
proc mark_line_endings
    push ax
    push bx
    push cx
    push dx

    mov dl, al                    ; save marker byte
    xor cx, cx                    ; CX = line index

mark_loop:
    cmp cx, [line_count]
    jae mark_done

    ; BX = line_lengths[cx] (word array so cx*2)
    mov bx, cx
    shl bx, 1
    mov bx, [line_lengths+bx]    ; BX = length of this line

    ; target = lines + (cx * MAX_LINELEN) + BX
    push bx                       ; save length
    mov ax, MAX_LINELEN
    mul cx                        ; AX = cx * MAX_LINELEN
    pop bx
    add ax, bx                    ; AX = offset into line
    add ax, offset lines          ; AX = final address

    mov bx, ax
    mov [bx], dl                  ; write marker (0Ah or 0)

    inc cx
    jmp mark_loop

mark_done:
    pop dx
    pop cx
    pop bx
    pop ax
    ret
endp
proc readfile

    mov ah,3Fh                ; read file
    mov bx, [filehandle]
    mov cx, 4096
    mov dx,offset readres
    int 21h
    mov [lengthr], ax         ; save number of bytes read

    ; Parse raw file data into lines buffer
    call parse_readres_into_lines

    ; --- Clear screen ---
    mov ah, 0
    mov al, 3                 ; 80x25 text mode
    int 10h

    ; --- Display header ---
	mov dx, offset header
	mov ah, 9h
	int 21h
	
	lea bx, [filename]
	add bl, [bx+1]        ; length
	mov [byte ptr bx+2], '$'
	
	mov dx, offset filename
	add dx, 2
	mov ah, 9h
	int 21h
	
	lea bx, [filename]
	add bl, [bx+1]
	mov [byte ptr bx+2], 0
	
    ; --- Reset editor state ---
    xor ax, ax
    mov [cur_line], ax
    mov [cur_col], ax
    mov [scroll_offset], ax
    mov [file_dirty], al
    mov [edit_counter], al
	
    ; --- Reset cursor to start position ---
    mov ah, 02h
    xor bh, bh
    mov dh, 1
    xor dl, dl
    int 10h
	
    ret
endp

proc save_file
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    ; Create/Truncate file to clear existing contents
    mov ah, 3Ch
    mov cx, 0                   ; normal file attribute
    mov dx, offset filename+2
    int 21h
    jc save_done                ; exit if error
    mov [filehandle], ax

    xor cx, cx                  ; CX = current line index
save_loop:
    cmp cx, [line_count]
    jae save_done

    mov bx, cx
    shl bx, 1
    mov dx, [line_lengths + bx] ; DX = length of current line

    cmp dx, 0
    je write_newline            ; skip writing chars if line is empty

    ; Calculate start of line in 'lines' buffer
    mov ax, MAX_LINELEN
    push dx                     ; save length
    mul cx                      ; AX = cx * MAX_LINELEN
    add ax, offset lines
    pop dx                      ; restore length in DX
    
    ; Write line characters
    push cx                     ; save line index
    mov cx, dx                  ; CX = number of bytes to write
    mov dx, ax                  ; DX = address of buffer
    mov ah, 40h                 ; write to file
    mov bx, [filehandle]
    int 21h
    pop cx                      ; restore line index

write_newline:
    ; Don't write newline after the last line
    mov ax, cx
    inc ax
    cmp ax, [line_count]
    jae next_line

    ; Write CRLF
    mov ah, 40h
    mov bx, [filehandle]
    push cx                     ; save line index
    mov cx, 2                   ; write 2 bytes
    mov dx, offset crlf_str
    int 21h
    pop cx                      ; restore line index

next_line:
    inc cx
    jmp save_loop

save_done:
    ; Close the file
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


; ============================================================
; render_screen
; Renders the editor content from the lines buffer to video memory.
; Clears screen, draws header, and displays visible lines.
; W.I.P currently does not work
; ============================================================
proc render_screen
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	
	; --- Clear screen (rows 0-24) ---
	mov ah, 0
	mov al, 3
	int 10h
	
	; --- Set up video memory ---
	mov ax, 0B800h
	mov es, ax
	xor di, di
	
	; --- Draw header on row 0 ---
	mov si, offset header
	mov ah, 07h              ; attribute
	xor di, di               ; Start at video offset 0
	
header_loop:
	lodsb
	cmp al, '$'
	je header_done
	mov [es:di], al
	mov [es:di+1], ah
	add di, 2
	jmp header_loop
	
header_done:
	; Add filename to header
	lea bx, [filename]
	add bl, [bx+1]
	mov [byte ptr bx+2], '$'
	
	mov si, offset filename
	add si, 2
	
header_name_loop:
	lodsb
	cmp al, '$'
	je header_name_done
	mov [es:di], al
	mov [es:di+1], ah
	add di, 2
	jmp header_name_loop
	
header_name_done:
	; Restore ASCIIZ
	lea bx, [filename]
	add bl, [bx+1]
	mov [byte ptr bx+2], 0
	
	mov ah, 02h
	mov dl, 0Ah
	int 21h
	
	
	mov bx,  MAX_LINELEN * 23
	add bx, offset lines
	mov [byte ptr bx], '$'
	mov ah, 09h
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

; ============================================================
; update_cursor
; Positions the hardware cursor based on cur_line, cur_col, scroll_offset.
; ============================================================
proc update_cursor
	push ax
	
	; Calculate screen row: cur_line - scroll_offset + 1
	mov ax, [cur_line]
	sub ax, [scroll_offset]
	inc ax
	
	
	; Column is directly from cur_col
	mov dx, [cur_col]
	
	mov dh, al ; save screen row
	
	; Set cursor position
	mov ah, 02h
	xor bh, bh
	int 10h
	
	pop ax
	ret
endp

proc main_loop
    mov ah, 0 ;clear screen
    mov al, 3
    int 10h
    
    call getFile
    call checkfile
	
    
    mov dl, 0ah
    mov ah, 2h
    int 21h
    
    call readfile
	; Note: readfile now initializes cur_line, cur_col, scroll_offset
	; Render the screen from the lines buffer
	call render_screen

    mov dh, 1      ; start at row 1
    mov dl, 0      ; start at column 0
    
    read_key:
		mov ah, 00h
		int 16h

		cmp al, 0
		je handle_extended
		cmp al, 08h    ; AL = 08h → backspace ASCII
		je handle_backspace
		cmp al, 0Dh    ; Enter key ASCII
		je handle_enter
		cmp al, 1Bh
		je quit
		cmp al, 13h
		je quit

		; Normal printable key
		; AL = ASCII
		jmp normal

		handle_extended:
			; AH already contains scan code
			cmp ah, 48h
			je up_arrow
			cmp ah, 50h
			je down_arrow
			cmp ah, 4Bh
			je left_arrow
			cmp ah, 4Dh
			je right_arrow
			cmp ah, 47h
			je home
			cmp ah, 4Fh
			je endkey
			jmp read_key
	home:
			mov [cur_col], 0
			call update_cursor
			jmp read_key
	endkey:
		push bx
		mov bx, [cur_line]
		shl bx, 1
		add bx, offset line_lengths
		movm2m [cur_col], [bx]
		pop bx
		call update_cursor
		jmp read_key
		
	handle_backspace:
		cmp [cur_col], 0
		je backspace_start_of_line
		
		dec [cur_col]             ; Move left to point to the deleted char

		push ax
		push bx
		push cx
		push dx
		push si
		push di

		; 1. Calculate chars to shift left
		mov bx, [cur_line]
		shl bx, 1
		mov cx, [line_lengths + bx]
		mov ax, [cur_col]
		sub cx, ax
		dec cx                  ; CX = line_lengths - cur_col - 1
		
		mov ax, [cur_line]
		mov dx, MAX_LINELEN
		mul dx
		add ax, [cur_col]
		add ax, offset lines
		mov di, ax              ; DI = dest (deleted char)
		mov si, ax
		inc si                  ; SI = src (char after deleted)
		
		cmp cx, 0
		jle skip_bs_shift

		; Shift characters left functionally
		push cx
		push es
		mov ax, ds
		mov es, ax
		cld
		rep movsb
		pop es
		pop cx

	skip_bs_shift:
		; Blank the final char location (DI naturally increments to the gap)
		mov [byte ptr di], ' '

		; Decrement line length
		mov bx, [cur_line]
		shl bx, 1
		dec [word ptr line_lengths + bx]

		; 2. Visually shift left and redraw
		mov ax, 0B800h
		mov es, ax
		
		mov ax, [cur_line]
		mov dx, 160
		mul dx
		add ax, 160             ; Skip header row
		
		mov bx, [cur_col]
		shl bx, 1
		add ax, bx
		mov di, ax              ; DI = video dest
		mov si, ax
		add si, 2               ; SI = video src
		
		cmp cx, 0
		jle skip_vis_bs_shift
		
		push cx
	vis_bs_shift_loop:
		mov al, [es:si]
		mov [es:di], al
		mov al, [es:si+1]
		mov [es:di+1], al
		add si, 2
		add di, 2
		loop vis_bs_shift_loop
		pop cx
		
	skip_vis_bs_shift:
		; Blank the last visual char
		mov [byte ptr es:di], ' '
		mov [byte ptr es:di+1], 07h

		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
		
		call update_cursor
		jmp read_key

	backspace_start_of_line:
		cmp [cur_line], 0
		je backspace_ignore        ; line 0 col 0 do nothing

		; Get length of prev line
		mov bx, [cur_line]
		dec bx
		shl bx, 1
		mov ax, [line_lengths + bx]
		mov cx, ax                 ; CX = length of prev line
		
		; Get length of current line
		mov bx, [cur_line]
		shl bx, 1
		mov ax, [line_lengths + bx] ; AX = length of current line
		
		; Check if combination exceeds MAX_LINELEN
		push ax
		add ax, cx
		cmp ax, MAX_LINELEN
		pop ax
		ja backspace_ignore        ; Too long to join
		
		; Set new cur_col to end of prev line before joining
		mov [cur_col], cx

		; Copy chars from current line to previous line
		cmp ax, 0
		je join_empty_line         ; Nothing to copy

		push cx                    ; save prev len
		push ax                    ; save current len

		; Source: lines[cur_line * MAX_LINELEN]
		mov ax, [cur_line]
		mov dx, MAX_LINELEN
		mul dx
		add ax, offset lines
		mov si, ax

		; Dest: lines[(cur_line - 1) * MAX_LINELEN + prev_len]
		mov ax, [cur_line]
		dec ax
		mov dx, MAX_LINELEN
		mul dx
		pop dx                     ; restore current len into dx
		pop cx                     ; restore prev len into cx
		add ax, cx
		add ax, offset lines
		mov di, ax

		push cx
		push dx
		
		mov cx, dx
		mov ax, ds
		mov es, ax
		rep movsb

		pop dx
		pop cx

	join_empty_line:
		; Update prev line length
		mov bx, [cur_line]
		dec bx
		shl bx, 1
		mov ax, cx
		add ax, dx
		mov [line_lengths + bx], ax

		; Shift subsequent lines UP
		mov ax, [cur_line]
		inc ax
		mov cx, [line_count]
		sub cx, ax
		jle update_line_count_bs   ; If no lines below, skip shift

		; Shift line_lengths array
		mov bx, [cur_line]
		shl bx, 1
		add bx, offset line_lengths
		mov di, bx
		mov si, bx
		add si, 2

		push cx
	shift_lengths_up_loop:
		mov ax, [si]
		mov [di], ax
		add si, 2
		add di, 2
		loop shift_lengths_up_loop
		pop cx

		; Shift text lines UP
		mov ax, [cur_line]
		mov dx, MAX_LINELEN
		mul dx
		add ax, offset lines
		mov di, ax
		mov si, ax
		add si, MAX_LINELEN
		
		mov ax, ds
		mov es, ax

	shift_text_up_loop:
		push cx
		push si
		push di
		
		mov cx, MAX_LINELEN / 2
		rep movsw
		
		pop di
		pop si
		pop cx
		add si, MAX_LINELEN
		add di, MAX_LINELEN
		loop shift_text_up_loop

	update_line_count_bs:
		dec [line_count]

		; Clear the last line that was shifted
		mov ax, [line_count]
		mov dx, MAX_LINELEN
		mul dx
		add ax, offset lines
		mov di, ax
		mov cx, MAX_LINELEN / 2
		xor ax, ax
		rep stosw

		; Update cur_line
		dec [cur_line]

		; Scroll adjust if cur_line is above viewport
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

	handle_enter:
		mov ax, [line_count]
		cmp ax, MAX_LINES
		jae enter_done

		mov cx, [line_count]
		sub cx, [cur_line]
		dec cx
		jl split_current_line_only
		jz split_current_line_only

		; Shift line_lengths array
		mov bx, [line_count]
		dec bx
		shl bx, 1
		add bx, offset line_lengths
		
		push cx
	shift_lengths_loop:
		mov ax, [bx]
		mov [bx + 2], ax
		sub bx, 2
		loop shift_lengths_loop
		pop cx

		; Shift text lines
		mov ax, [line_count]
		dec ax
		mov dx, MAX_LINELEN
		mul dx
		add ax, offset lines
		mov si, ax
		mov di, ax
		add di, MAX_LINELEN

		mov ax, ds
		mov es, ax  ; ensure es=ds for rep movsw
		
	shift_text_lines_loop:
		push cx
		push si
		push di
		
		mov cx, MAX_LINELEN / 2
		rep movsw
		
		pop di
		pop si
		pop cx
		sub si, MAX_LINELEN
		sub di, MAX_LINELEN
		loop shift_text_lines_loop

	split_current_line_only:
		mov ax, ds
		mov es, ax

		; Clear the new line (cur_line + 1)
		mov ax, [cur_line]
		inc ax
		mov dx, MAX_LINELEN
		mul dx
		add ax, offset lines
		mov di, ax
		mov cx, MAX_LINELEN / 2
		xor ax, ax
		rep stosw       ; clears MAX_LINELEN bytes at new line with 0

		mov bx, [cur_line]
		shl bx, 1
		mov cx, [line_lengths + bx]
		sub cx, [cur_col]

		mov ax, cx
		cmp ax, 0
		jge set_new_length
		xor ax, ax
		mov cx, 0
	set_new_length:
		mov bx, [cur_line]
		inc bx
		shl bx, 1
		mov [line_lengths + bx], ax

		mov ax, [cur_col]
		mov bx, [cur_line]
		shl bx, 1
		mov [line_lengths + bx], ax

		cmp cx, 0
		jle finalize_enter

		; Copy Characters
		mov ax, [cur_line]
		mov dx, MAX_LINELEN
		mul dx
		add ax, [cur_col]
		add ax, offset lines
		mov si, ax

		mov ax, [cur_line]
		inc ax
		mov dx, MAX_LINELEN
		mul dx
		add ax, offset lines
		mov di, ax

	copy_split_loop:
		mov al, [si]
		mov [di], al
		mov [byte ptr si], 0 ; Clear old char
		inc si
		inc di
		loop copy_split_loop

	finalize_enter:
		inc [line_count]
		inc [cur_line]
		mov [cur_col], 0

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
		jmp read_key
		
	normal:
		push cx
		push bx
		push ax
		push si
		push di
		
		mov bx, [cur_line]
		shl bx, 1
		mov cx, [line_lengths+bx]
		
		; Ensure we don't exceed MAX_LINELEN
		cmp cx, MAX_LINELEN
		jae normal_done_full
		
		inc [word ptr line_lengths+bx] ; increment the line length

		; Calculate offset of the end of the current line
		mov ax, [cur_line]
		mov dx, MAX_LINELEN
		mul dx
		add ax, cx             ; CX is the old line length
		add ax, offset lines
		mov di, ax             ; DI points to end of line + 1 (where the last char should move)
		mov si, ax
		dec si                 ; SI points to the last actual char of the line
		
		mov dx, cx             ; DX = old line length
		sub dx, [cur_col]      ; DX = number of chars to shift
		
		cmp dx, 0
		jle skip_func_shift    ; if cur_col >= old line length, no shifting needed

		push cx                ; save line length
		mov cx, dx             ; CX = count to shift
		
		; Shift chars right (functionally)
		; Need to iterate backwards so rep movsb requires STD
		push es
		mov ax, ds
		mov es, ax
		std
		rep movsb
		cld
		pop es
		pop cx
		
	skip_func_shift:
		; Insert the new char functionally
		mov ax, [cur_line]
		mov dx, MAX_LINELEN
		mul dx
		add ax, [cur_col]
		add ax, offset lines
		mov bx, ax
		
		; Retrieve the character to insert from stack
		; Stack: si, di, ax, bx, cx -> ax is [sp+4]
		mov bp, sp
		mov al, [bp+4] 
		mov [byte ptr bx], al
		
		; Visually redraw the rest of the line from cur_col onwards
		mov ax, 0B800h
		mov es, ax
		
		mov ax, [cur_line]
		mov dx, 160
		mul dx
		add ax, 160             ; Skip header row
		mov bx, [cur_col]
		shl bx, 1
		add ax, bx
		mov di, ax              ; DI = video memory destination
		
		mov ax, [cur_line]
		mov dx, MAX_LINELEN
		mul dx
		add ax, [cur_col]
		add ax, offset lines
		mov si, ax              ; SI = lines array source

		; Calculate how many characters to redraw: (old line length + 1) - cur_col
		mov cx, dx              ; DX has old len - cur_col ?? Wait no, let's recalculate
		mov bx, [cur_line]
		shl bx, 1
		mov cx, [line_lengths+bx] ; CX = new line length
		sub cx, [cur_col]       ; CX = number of chars to redraw
		
	redraw_vis_loop:
		mov al, [si]
		mov [es:di], al
		mov [byte ptr es:di+1], 07h
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
		pop di
		pop si
		pop ax
		pop bx
		pop cx
		jmp read_key
		
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
    
    up_arrow:
    ; 1. Check if we are already at the top (line 0)
    cmp [cur_line], 0
    je read_key             ; If at the first line, do nothing

    ; 2. Move one line up
    dec [cur_line]

    ; 3. Get the length of the NEW line we just entered
    mov bx, [cur_line]
    shl bx, 1               ; Word-array index (BX * 2)
    mov ax, [line_lengths+bx] ; AX = length of the line above

    ; 4. Check if current column exceeds the new line's length
    cmp [cur_col], ax
    jbe skip_col_upd_up     ; If cur_col <= line length, keep it
    mov [cur_col], ax       ; Else, snap to the end of the line

	skip_col_upd_up:
		; 5. Handle Viewport Scrolling (Moving up)
		; If the new line is above the current scroll_offset, we need to scroll up
		mov ax, [scroll_offset]
		cmp [cur_line], ax
		jb scroll_up            ; If cur_line < scroll_offset, trigger scroll up

    ; 6. Update visual cursor and return
    call update_cursor
    jmp read_key

	scroll_up:
		dec [scroll_offset]
		call render_screen
		jmp read_key
		
    down_arrow:
    ; 1. Check if we are already at the last line in the buffer
    mov ax, [line_count]
    dec ax
    cmp [cur_line], ax
    je read_key             ; If current line == last index, do nothing

    ; 2. Move to the next line
    inc [cur_line]

    ; 3. Get the length of the NEW current line
    mov bx, [cur_line]
    shl bx, 1               ; Multiply by 2 for word-array indexing
    mov ax, [line_lengths+bx] ; AX = actual character count of the new line

    ; 4. Check if our horizontal position exceeds the new line's length
    ; If cur_col is further right than the text on this line, snap it back
    cmp [cur_col], ax
    jbe skip_col_upd        ; If cur_col <= line length, keep current column
    mov [cur_col], ax       ; Else, set column to line length (end of line)

	skip_col_upd:
		; 5. Handle Viewport Scrolling
		; Check if the new cursor position is below the current screen (24 lines visible)
		mov ax, [scroll_offset]
		add ax, 23              ; Last visible row on screen (0-23)
		cmp [cur_line], ax
		ja scroll_down          ; If cursor is below viewport, trigger scroll

    ; 6. Refresh cursor position on screen
    call update_cursor
    jmp read_key

	scroll_down:
		inc [scroll_offset]
		call render_screen
		jmp read_key
		
    left_arrow:
        cmp [cur_col], 0
        ja notup
		cmp [cur_line], 0
		je read_key
		;; move to previous line ;;
		
		;calculate the end of the previous line
		;/=======================\;
		dec [cur_line]
		mov bx, offset line_lengths
		push [cur_line]
		shl [cur_line], 1 ;multiply (shift bits left by one) for word-array indexing
		add bx, [cur_line] ;add the offset to get the address of the line length for the needed line
		;\=======================/;
		movm2m [cur_col], [bx] 		;set column to the end of the previous line
		pop [cur_line] ;restore line value to the correct one
		jmp update_cursor_pos

		;; case for just moving left ;;
    notup:
        dec [cur_col]
		jmp update_cursor_pos
		
    right_arrow:
        ; 1. Get current line length
        mov bx, [cur_line]
        shl bx, 1
        mov ax, [line_lengths + bx]

        ; 2. Check if we are at the end of the current line
        cmp [cur_col], ax
        jb move_right_same_line

        ; 3. If at the end, try to wrap to the next line
        mov ax, [line_count]
        dec ax
        cmp [cur_line], ax
        jae read_key             ; Already at the last line, nowhere to go right

        ;; Move to start of next line ;;
        inc [cur_line]
        mov [cur_col], 0
        jmp update_cursor_pos

    move_right_same_line:
        inc [cur_col]
        jmp update_cursor_pos
		
update_cursor_pos:
		; Check if need to scroll
		mov ax, [cur_line]
		cmp ax, [scroll_offset]
		jb cursor_left_of_viewport
		mov ax, [scroll_offset]
		add ax, 23
		cmp [cur_line], ax
		ja cursor_right_of_viewport
		; Within viewport - just update cursor
		mov ax, [cur_line]
        shl ax, 1 ; multiply by 2 (shift all bits left by one)
        mov bx, ax
        mov ax, [line_lengths+bx]
		cmp [cur_col], ax
		jna upd
		mov [cur_col], ax
	upd:
		call update_cursor
		jmp read_key
cursor_left_of_viewport:
		mov [scroll_offset], 0
		call render_screen
		jmp read_key
cursor_right_of_viewport:
		mov ax, [cur_line]
		sub ax, 23
		mov [scroll_offset], ax
		call render_screen
		jmp read_key
		
	quit:
		call save_file
		; clear screen
		mov ah, 0
		mov al, 3
		int 10h

		; move cursor to top-left
		mov dh, 0
		mov dl, 0
		goto_pos dh, dl

		; exit program properly
		mov ax, 4C00h
		int 21h
		
    ret
endp
start:
	mov ax, @data
	mov ds, ax
	
	call main_loop
	;mov ah, 02h
    ;xor bh, bh
    ;mov dh, 1
    ;xor dl, dl
    ;int 10h
	
exit:
	mov ax, 4c00h
	int 21h
END start
