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
CODESEG	
macro goto_pos row, col
    mov ah, 02h
    mov dh, row
    mov dl, col
    int 10h
endm
macro movm2m op1, op2
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
			jmp read_key
	handle_backspace:
		cmp [cur_col], 0
		je left_arrow             ; Can't backspace if already at the start of the line
		
		dec [cur_col]           ; Move left to point to the character to be deleted

		push ax
		push bx
		push cx

		; 1. Calculate the byte offset in the 'lines' buffer
		mov ax, [cur_line]
		mov bx, [cur_col]
		mov cx, 80
		mul cx 
		add ax, bx              ; AX = offset within 'lines' array
		mov [char_location], ax ; Save it for video memory calculation

		; 2. Clear the character in the internal buffer
		mov bx, ax
		add bx, offset lines
		mov [byte ptr bx], ' '  ; Set to space (ASCII 20h)

		; 3. Clear the character visually on the screen
		mov ax, 0B800h
		mov es, ax
		mov ax, [char_location]
		mov cx, 2
		mul cx
		add ax, 160             ; Skip the 80 characters of the header (row 0)
		mov di, ax
		mov [byte ptr es:di], ' '   ; Write space character
		mov [byte ptr es:di+1], 07h ; Set normal attribute (light gray)

		; 4. Update the line length count
		mov bx, [cur_line]
		shl bx, 1               ; Word index (BX * 2)
		dec [word ptr line_lengths + bx]

		pop cx
		pop bx
		pop ax
		
		call update_cursor      ; Refresh the hardware cursor position
		jmp read_key            ; Wait for the next input
		
	normal:

		push cx
		push bx
		push ax
		
		mov ax, 0B800h
		mov es, ax
		mov ax,[cur_line] ; edit array for the chars
		mov bx, [cur_col]
		mov cx, 80
		mul cx 
		add ax, bx
		mov [char_location], ax ; save location to update screen
		mov bx, ax
		pop ax
		push ax
		add bx, offset lines
		mov [byte ptr bx], al
		mov ax, [char_location]
		mov cx, 2
		mul cx
		add ax, 160
		mov di, ax
		pop ax
		mov cx , 07h
		mov [es:di], al
		mov bx, [cur_line]
		shl bx, 1                 ; multiply by 2
		mov cx, [line_lengths+bx]
		cmp [cur_col],cx
		jb in_line
		inc [word ptr bx + line_lengths]
		in_line:
		inc [cur_col]
		call update_cursor_pos
		pop bx
		pop cx
		
	
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
