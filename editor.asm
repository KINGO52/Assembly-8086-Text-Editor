IDEAL
MODEL small
STACK 100h
DATASEG
	msg db 'Enter filname (including file extension) $'
	filename db 64
			db ?
			db 64 dup (?)
	emptyname db 'you entered nothing...$'
	erroropening db 'there was an error while opening the file did you perhaps enter the wrong name?$'
	filehandle dw ?
	readres db 4096 dup (?)
	lengthr dw ?
; --------------------------
; Your variables here
; --------------------------
CODESEG	
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

	mov ah,3Fh                ;read file
	mov bx, [filehandle]
	mov cx, 4096
	mov dx,offset readres
	int 21h
	mov [lengthr], ax
	
	mov bx, offset readres ; add $ at the end of the buffer
	add bx, [lengthr]
	mov [byte ptr bx], '$'
	mov dx, offset readres
	mov ah, 9h
	int 21h
	
	ret

endp 
start:
	mov ax, @data
	mov ds, ax
	
	mov ah, 0 ;clear screen
	mov al, 2
	int 10h
	
	call getFile
	call checkfile
	
	mov dl, 0ah
	mov ah, 2h
	int 21h
	
	call readfile
; --------------------------
; Your code here
; --------------------------
	
exit:
	mov ax, 4c00h
	int 21h
END start
