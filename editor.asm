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
	MAX_LINELEN   equ 79          ; Max characters per line (for 80-col display)
	; Memory usage: (200 * 79) + (200 * 2) = 15800 + 400 = 16200 bytes.

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
	; DEPRECATED: currentrow, cursrow, curscol - replaced by cur_line, cur_col, scroll_offset
	; currentrow db ?
	; cursrow db ?
	; curscol db ?
CODESEG	
macro goto_pos row, col
    mov ah, 02h
    mov dh, row
    mov dl, col
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

; ============================================================
; parse_readres_into_lines
; Parses raw file content from readres into the lines buffer.
; Handles CR, LF, and CRLF line endings.
; Updates line_count with the number of lines parsed.
; ============================================================
proc parse_readres_into_lines
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push si
	push di
	
	xor bx, bx                    ; BX = line index (0-based)
	xor di, di                    ; DI = offset within current line (0-based)
	
	mov si, offset readres        ; SI = pointer to raw file data
	mov cx, [lengthr]             ; CX = number of bytes read
	
	or cx, cx
	jz parse_done                 ; if no bytes read, done
	
parse_char:
	lodsb
	
	cmp al, 0
	je parse_char                 ; skip null bytes
	
	cmp al, 0Dh                  ; CR
	je handle_crlf
	
	cmp al, 0Ah                  ; LF
	je handle_lf
	
	; Regular character - store in current line
	cmp di, MAX_LINELEN - 1
	jae parse_char                ; skip if line is full (leave room for null)
	
	; Calculate destination offset: line_index * MAX_LINELEN + di
	push bx
	mov ax, MAX_LINELEN
	mul bx
	mov si, offset lines
	add si, ax
	add si, di
	pop bx
	
	mov [si], al                 ; store character
	inc di
	
	jmp parse_char

handle_crlf:
	; Store line length for current line
	push bx
	mov ax, MAX_LINELEN
	mul bx
	mov si, offset line_lengths
	add si, bx
	add si, bx
	mov [si], di                  ; store line length
	pop bx
	
	inc bx                        ; next line
	cmp bx, MAX_LINES
	jae parse_done                ; stop if max lines reached
	
	xor di, di                    ; reset column offset
	
	; Check for LF after CR (CRLF case)
	mov si, offset readres
	lodsb
	cmp al, 0Ah
	je parse_char                 ; if LF, already consumed it
	
	; Not CRLF, put AL back
	dec si
	jmp parse_char

handle_lf:
	; Store line length for current line
	push bx
	mov ax, MAX_LINELEN
	mul bx
	mov si, offset line_lengths
	add si, bx
	add si, bx
	mov [si], di
	pop bx
	
	inc bx
	cmp bx, MAX_LINES
	jae parse_done
	
	xor di, di
	jmp parse_char

parse_done:
	; Store final line length if any content
	cmp di, 0
	je store_count
	push bx
	mov ax, MAX_LINELEN
	mul bx
	mov si, offset line_lengths
	add si, bx
	add si, bx
	mov [si], di
	pop bx
	inc bx

store_count:
	mov [line_count], bx
	
	pop di
	pop si
	pop cx
	pop bx
	pop ax
	pop bp
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
	
	; Pad rest of header row with spaces (80 chars = 160 bytes)
	cmp di, 160
	jae skip_header_pad
	mov cx, 160
	sub cx, di
	shr cx, 1
	mov ah, 07h
header_pad_loop:
	mov [es:di], ah
	add di, 2
	loop header_pad_loop
	
skip_header_pad:
	; --- Render visible lines (rows 1-24) ---
	mov cx, 24                 ; 24 visible rows
	xor bx, bx                 ; BX = screen row offset (0-23)
	
render_lines_loop:
	push cx
	push bx
	
	; Calculate buffer line index: scroll_offset + screen_row
	mov ax, [scroll_offset]
	add ax, bx
	mov si, ax                ; SI = buffer line index
	
	; Check if buffer_line < line_count
	cmp si, [line_count]
	jae render_empty_line
	
	; --- Render a line from buffer ---
	; Calculate offset in lines array: line_index * MAX_LINELEN
	mov ax, MAX_LINELEN
	mul si
	mov si, ax
	add si, offset lines      ; SI = pointer to line data
	
	; Get line length
	push bx
	mov ax, [scroll_offset]
	add ax, bx
	shl ax, 1
	mov bx, ax
	mov cx, [line_lengths+bx] ; CX = line length
	pop bx
	
	; Copy up to 80 characters
	mov di, bx
	shl di, 1
	add di, 160                ; DI = video offset for this row (row 1 = 160)
	
	mov ah, 07h               ; text attribute
	
	cmp cx, 80
	jbe copy_line
	mov cx, 80
	
copy_line:
	lodsb
	mov [es:di], al
	mov [es:di+1], ah
	add di, 2
	loop copy_line
	
	; Pad rest of line with spaces
	push bx
	mov ax, [scroll_offset]
	add ax, bx
	shl ax, 1
	mov cx, 80
	mov bx, ax
	sub cx, [line_lengths+bx]
	pop bx
	
pad_line_loop:
	cmp cx, 0
	jle render_next_line
	mov [es:di], ah
	add di, 2
	loop pad_line_loop
	jmp render_next_line
	
render_empty_line:
	; Render blank line
	mov di, bx
	shl di, 1
	add di, 160
	mov cx, 80
	mov ah, 07h
blank_line_loop:
	mov [es:di], ah
	add di, 2
	loop blank_line_loop
	
render_next_line:
	pop bx
	pop cx
	inc bx
	loop render_lines_loop
	
	; --- Update cursor position ---
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
    int 16h        ; wait for key
	
	
	
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
		jne read_key
		jmp quit
    

 
    jmp read_key
    
    up_arrow:
        cmp [cur_line], 0
        je read_key
		dec [cur_line]
		; Check if cursor is now above viewport
		mov ax, [cur_line]
		cmp ax, [scroll_offset]
		jb scroll_up
		; Just update cursor position
		call update_cursor
		jmp read_key
	scroll_up:
		dec [scroll_offset]
		call render_screen
		jmp read_key
		
    down_arrow:
        mov ax, [line_count]
		dec ax
        cmp [cur_line], ax
        je read_key
        inc [cur_line]
		; Check if cursor is now below viewport (scroll_offset + 23)
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
		
    left_arrow:
        cmp [cur_col], 0
        ja notup
		cmp [cur_line], 0
		je read_key
		dec [cur_line]
		mov ax, [line_lengths]
		add ax, ax
		mov bx, ax
		mov bx, [line_lengths+bx]
		mov [cur_col], bx
		jmp update_cursor_pos
		notup:
        dec [cur_col]
		jmp update_cursor_pos
		
    right_arrow:
        ; Get current line length
        mov ax, [cur_line]
        shl ax, 1
        mov bx, ax
        mov ax, [line_lengths+bx]
		dec ax
        cmp [cur_col], ax
        ja read_key
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
		mov ah, 0 ;clear screen
		mov al, 3
		int 10h
		mov dh, 0
		mov dl, 0
		goto_pos dh, dl
		
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
