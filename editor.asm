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
	currentrow db ?
	cursrow db ?
	curscol db ?
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
proc readfile

    mov ah,3Fh                ; read file
    mov bx, [filehandle]
    mov cx, 4096
    mov dx,offset readres
    int 21h
    mov [lengthr], ax         ; save number of bytes read

    ; --- Clear screen ---
    mov ah, 0
    mov al, 3                 ; 80x25 text mode
    int 10h

    ; --- Set up video memory ---
    mov ax, 0B800h
    mov es, ax
    xor di, di                ; DI = offset in screen memory
    xor bx, bx                ; BX = current column (0..79)
    xor dx, dx                ; DX = current row (0..24)

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
	
	lea bx, [filename]					;Re-add ASCIIZ termination
	add bl, [bx+1]        ; length
	mov [byte ptr bx+2], 0
	
	
	xor bx, bx               
    xor dx, dx  
	xor di,di ; set column to 0
    add di, 160                ; move to next row
	inc dx	; row++
	
    mov si, offset readres
    mov cx, [lengthr]
	mov [currentrow], 1
   

display_loop:
    lodsb                     ; AL = next character

    cmp al, 0
    je next_char              ; skip null bytes

    cmp al, 0Dh               ; CR
    je handle_cr

    cmp al, 0Ah               ; LF
    je handle_lf

    ; --- Normal character ---
    mov ah, 07h               ; text attribute
    mov [es:di], al           ; character
    mov [es:di+1], ah         ; attribute
    add di, 2
    inc bx                     ; next column

    cmp bx, 80
    jb next_char               ; still within line
    jmp handle_cr              ; wrap line (same as carriage return)
	
handle_cr:
	xor bx, bx                ; reset column counter
	inc [currentrow]
	xor di,di ; set column to 0
	mov al, 160
	mul [currentrow]
	mov di, ax
	
    inc dx	; row++
    cmp dx, 25
    jb next_char
    jmp done_display

handle_lf:
	cmp bx, 0                 ; check if at start of line
    je next_char              ; if column 0, CR already moved us - skip LF
    jmp handle_cr             ; otherwise treat LF as line break (LF-only files)

next_char:
    loop display_loop
    jmp done_display

done_display:
    ; --- Reset cursor to top-left ---
    mov ah, 02h
    xor bh, bh
    mov dh, 1
    xor dl, dl
    int 10h
	
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
        cmp dh, 2
        jb read_key
		dec [cursrow]
        goto_pos cursrow,curscol
        jmp read_key
    down_arrow:
        cmp dh, 24
        je read_key
        inc [cursrow]
        goto_pos cursrow,curscol
        jmp read_key
    left_arrow:
        cmp dl, 0
        ja notup
		cmp dh, 1
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
        cmp dl, 79
        jb notdown
		cmp dh, 24
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
