; current goals or problems
;adding editing functionality
;saving the buffer into the file as well as saving into a temp file on every edit


JUMPS
IDEAL
MODEL small
STACK 100h
DATASEG
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

; --- DATA MODEL CONSTANTS ---
MAX_LINES     equ 200         ; Max number of lines in the buffer
MAX_LINELEN   equ 128         ; Max characters per line (excluding CR/LF)
; Memory usage: (200 * 128) + (200 * 2) = 25600 + 400 = 26000 bytes.

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
CODESEG	
macro goto_pos row, col
    mov ah, 02h
    mov dh, [row]
    mov dl, [col]
    int 10h
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
proc parse_readres_into_lines
    ; uses: SI -> offset readres, CX -> bytes read (from lengthr)
    ; outputs: fills lines, line_lengths, line_count
    ; preserves: AX, BX, CX, DX, DI, SI, BP, ES, DS (will push/pop)

    push ax
    push bx
    push cx
    push dx
    push di
    push si
    push ds
    push es

    ; Setup DS for lines buffer (all data is in DS for small model)
    ; (DS should already be pointing to @data from start:, no need to set ES if not for video)

    mov word ptr [line_count], 0 ; Initialize line_count to 0
    mov word ptr [cur_line], 0   ; Reset cur_line
    mov word ptr [cur_col], 0    ; Reset cur_col

    mov si, offset readres       ; Source for reading characters from readres
    mov di, offset lines         ; Destination for writing characters to lines buffer
    mov bx, 0                    ; bx will hold current_line_len (for current line being parsed)
    xor ah, ah                   ; Clear AH for character processing

    ; Loop through readres buffer
    parse_loop:
        cmp si, offset readres   ; Check if SI has advanced past the initial offset
        jge check_lengthr        ; Only check against lengthr if SI is valid
        jmp end_parse_loop       ; Should not happen if lengthr is 0 initially

    check_lengthr:
        mov ax, si
        sub ax, offset readres   ; ax = bytes processed so far
        cmp ax, [lengthr]        ; Have we processed all bytes from readres?
        jge end_parse_loop

        cmp word ptr [line_count], MAX_LINES ; Check if we exceed MAX_LINES
        jge end_parse_loop_full

        lodsb                    ; AL = next character from readres (DS:SI)

        cmp al, 0Dh              ; Is it CR?
        je handle_line_end

        cmp al, 0Ah              ; Is it LF?
        je handle_line_end

        ; --- Normal character ---
        cmp bx, MAX_LINELEN - 1  ; Check if current line is full (MAX_LINELEN-1 for null terminator or future use)
        jge next_char_truncate   ; Truncate if line is full

        mov [di], al             ; Store character in lines buffer
        inc di                   ; Move to next position in lines buffer
        inc bx                   ; Increment current_line_len

        jmp next_char_continue

    handle_line_end:
        ; Check if the previous char was CR and current is LF to handle CRLF
        cmp al, 0Ah              ; Is current char LF?
        jne not_crlf_second      ; If not LF, it's CR-only or LF-only.

        ; If previous char was CR and current is LF, SI already advanced. Check previous char that was just read (si-1)
        cmp byte ptr [si-2], 0Dh ; Check char before the current LF, which was the CR
        je consume_crlf_lf       ; If previous was CR, then this LF is part of CRLF.

    not_crlf_second:
        ; Save current line length
        mov ax, word ptr [line_count]
        mov [line_lengths + ax*2], bx ; Store bx (current_line_len) into line_lengths array (each entry is a word)

        ; Reset current_line_len for next line
        mov bx, 0

        ; Increment line_count
        inc word ptr [line_count]

        ; Move DI to the start of the next physical line in the lines buffer
        mov ax, word ptr [line_count]
        mov dx, MAX_LINELEN
        mul dx                   ; ax = line_count * MAX_LINELEN
        add ax, offset lines     ; ax = absolute offset of start of next line
        mov di, ax               ; DI points to start of new line

    consume_crlf_lf:
        ; If it was a CRLF, the LF is consumed here, and we continue parsing.
        ; If it was just CR or LF, the line break is handled and we continue.

    next_char_continue:
        jmp parse_loop

    next_char_truncate:
        ; If line is full, just skip current character until next line break
        jmp next_char_continue

    end_parse_loop_full:
        ; Handle case where MAX_LINES is reached. Remaining characters are ignored.
        ; (Could add a warning here if needed)
        jmp end_parse_loop

    end_parse_loop:
        ; Save the last line if it's not empty and no line end was found
        cmp bx, 0
        je skip_last_line_save ; If last line is empty, no need to save

        cmp word ptr [line_count], MAX_LINES ; Check if we still have space for one more line
        jge skip_last_line_save_full

        mov ax, word ptr [line_count]
        mov [line_lengths + ax*2], bx ; Store current_line_len for the last line

        inc word ptr [line_count] ; Increment line_count for the last line

    skip_last_line_save:
    skip_last_line_save_full:

    pop es
    pop ds
    pop si
    pop di
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

    ; --- Clear screen ---
    ; Old screen display logic removed.
    call parse_readres_into_lines
    ; call render_screen ; Placeholder for Phase 3

	
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
	mov [cursrow], 1
	mov [curscol], 0

    mov dh, 1      ; start at row 1
    mov dl, 0      ; start at column 0
    
    read_key:
    mov ah, 00h
    int 16h        ; wait for key
	
	
	
	special keys:
		cmp ah, 48h
		je up_arrow
		cmp ah, 50h
		je down_arrow
		cmp ah, 4Bh
		je left_arrow
		cmp ah, 4Dh
		je right_arrow
		cmp al, 13h
		jne read_key
		jmp quit
    


    jmp read_key
    
    up_arrow:
        cmp [cursrow], 2
        jb read_key
		dec [cursrow]
        goto_pos cursrow,curscol
        jmp read_key
    down_arrow:
        cmp [cursrow], 24
        je read_key
        inc [cursrow]
        goto_pos cursrow,curscol
        jmp read_key
    left_arrow:
        cmp [curscol], 0
        ja notup
		cmp [cursrow], 1
		je read_key
		dec [cursrow]
		mov [curscol], 79
		goto_pos cursrow, curscol
		jmp read_key
		notup:
        dec [curscol]
        goto_pos cursrow,curscol
        jmp read_key
    right_arrow:
        cmp [curscol], 79
        jb notdown
		cmp [curscol], 24
		je read_key
		inc [cursrow]
		mov [curscol], 0
		goto_pos cursrow, curscol
		jmp read_key
		notdown:
        inc [curscol]
        goto_pos cursrow,curscol
        jmp read_key
		
	quit:
		mov ah, 0 ;clear screen
		mov al, 3
		int 10h
		mov [cursrow], 0
		mov [curscol], 0
		goto_pos cursrow,curscol
		
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
