%include "AtomicOs.inc" 	; functions defenitions

%macro cmd 2
	db %1
	dw %2
%endm

org 0h

;sence we only own the momory we are on 512 bytes we request more
	
	mov bx,GetMem
	int 21h
		
	mov [FarSeg+2],si
	
	
	
Debug:
	
	mov si,Debug_help
	mov bx,OutStr
	int 21h
	
	
	
	
	
	
new_cmd:
	
	mov es,[FarSeg+2]
	
	
	
	mov al,'-'
	mov bx,OutChar
	int 21h
	mov si,Buffer
	mov dx,BufferSize
	mov bx,InStr
	int 21h
	
	mov si,Buffer

new_char:
	lodsb
	cmp al,' '
	jz new_char
	cmp al,'?'
	jz Debug
	or al,20h		; convert to lowercase
	mov di,cmd_table
check_cmd:
	cmp al,[di]
	jnz no_cmd
	call word [di+1]
	jmp new_cmd
no_cmd:
	add di,byte 3
	cmp di,cmd_table_end
	jb check_cmd
	cmp al,'q'
	jnz new_cmd
	
	mov es,[FarSeg+2]
	mov bx,FreeMem
	int 21h
	
	
	retf			; back to kernel

do_dump:
	call InHex
	mov si,ax
	call ShowWord
	mov cx,16
dump_byte:
	mov al,' '
	mov bx,OutChar
	int 21h
	es lodsb
	call ShowByte
	loop dump_byte
	mov bx,NewLine
	int 21h
	ret

do_enter:
	call InHex
	xchg ax,di
enter_new_byte:
	cmp byte [si-1],' '
	jb enter_end
	mov ah,[si]
	;cmp ah,"'"
	;jz enter_text
	cmp ah,'"'
	jnz enter_byte
enter_text:
	inc si
enter_char:
	lodsb
	cmp al,' '
	jb enter_end
	cmp al,ah
	jz enter_test_end
	stosb
	jmp enter_char
enter_test_end:
	inc si
	jmp enter_new_byte
enter_byte:
	call InHex
	stosb
	jmp enter_new_byte
enter_end:
	ret

do_fill:
	call InHex
	xchg ax,di
	call InHex
	xchg ax,bp
	call InHex
fill_byte:
	cmp di,bp
	stosb
	jb fill_byte
	ret

do_go:
	call InHex
	mov [FarSeg],ax
	push es
	pop ds
	call far [cs:FarSeg]		; program MUST end with retf
	push cs
	pop ds
	ret

do_move:
	call InHex
	push ax
	call InHex
	xchg ax,bp
	call InHex
	xchg ax,di
	pop si
move_byte:
	cmp si,bp
	es movsb
	jb move_byte
	ret

do_load:
	mov ah,2

do_disk:
	push ax
	call InHex
	xchg ax,di
	call InHex
	xchg ax,cx
	pop ax
	mov bx,DiskIO
	int 21h
	ret

do_write:
	mov ah,3
	jmp do_disk


;---------------------------------------------------------

InHex:
	; ds:si -> ASCII hex number
	; return: ax = number

	xor	bx,bx

skip_space:
	lodsb
	cmp	al,' '
	je	skip_space
	dec	si

NextDigit:
	; get char from buffer into AL:
	lodsb

	or	al,20h		; convert to lowercase

	cmp	al,'9'
	jbe	DigitOk
	cmp	al,'a'
	jb	StopInput
	sub	al,'a'-':'
DigitOk:
	sub	al,'0'		; convert from ASCII code:
	cmp	al,15		; ok if in range 0..15
	ja	StopInput

	; multiply bx by 16 and add al
	shl	bx,4
	or	bl,al

	jmp	NextDigit

StopInput:
	xchg	ax,bx
	ret	;Now we return

;---------------------------------------------------------

ShowWord:
	call	ShWo2
ShWo2:	xchg	al,ah

ShowByte:
	call	ShBy1
ShBy1:	rol	al,4
	push	ax
	and	al,0Fh
	cmp	al,0Ah		;If Al < 10, set CF = 1
	sbb	al,69h		;0-9: 96h..9hF,  A-F: A1h..A6h
	das			;0-9: subtr. 66h -> 30h..39h,
	mov	bx,OutChar
	int	21h		;A-F: subtr. 60h -> 41h..46h
	pop	ax
	ret

Debug_help:
db 'Debug commands:',0Dh,0Ah
db 'dump -D ofs',0Dh,0Ah
db 'enter-E ofs byte "txt"',0Dh,0Ah
db 'fill -F start end byte',0Dh,0Ah
db 'move -M start end dest',0Dh,0Ah
db 'go   -G ofs',0Dh,0Ah
db 'load -L ofs secno',0Dh,0Ah
db 'write-W ofs secno',0Dh,0Ah
db 'quit -Q',0Dh,0Ah
;db 'All numbers in hex',0Dh,0Ah
db 0

cmd_table:
	cmd 'd', do_dump
	cmd 'e', do_enter
	cmd 'f', do_fill
	cmd 'g', do_go
	cmd 'l', do_load
	cmd 'm', do_move
	cmd 'w', do_write
cmd_table_end:

FarSeg	dw 0000h,3000h		;user programs in segment 3000h:0000h

;Andre: we dont own memory abouve us so we 
;		have to have buffer inside our page

Buffer:
BufferSize equ 510 - ($-$$)

times 510-($-$$) db 0
dw 0AA55h

