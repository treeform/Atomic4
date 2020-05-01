

; this file is from Asko
; I converted it for Atomic Os 3



;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; TSRClock - Example Terminate and Stay Resident program for DebugOS
;
; Compile with nasm v0.98.38:
;   nasm -f bin -otsrclock.bin tsrclock.asm
; Install:
;   copyboot tsrclock.bin 3
; Run under DebugOS:
;   LFE00 3
;   GFE00
;
; Notes:
;   - load address in range 0..FE00
;   - can be moved before run. ex. L0 3  ->  M0 DF FF20  ->  GFF20
;     then TSR routine is in FFB6-FFFF
;   - don't overwrite tsr area while clock is running
;
; Version history:
;   0.01 - 30.Jun.2004 by ATV
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
bits 16
segment .text

;%include "debugfu.asm"		;functions
%include "Atomicos.inc"


Color		equ 20h 	;Green background with Black text
VGAMono 	equ 0B000h
VGAColor	equ 0B800h
Line		equ 1		;range 1 to 25
Column		equ 73		;range 1 to 80
Separator	equ 3Ah 	;':'

org	0000h

Start:
	call	Init

Init:
	
	pop	bp		;calc load offset
	sub	bp,byte 3	;bp = start offset
	
	
	xor	ax,ax
	mov	ds,ax
	
	cli
	
	lea	ax,[bp+NewInt1C-Start]
	lea	bx,[bp+OldInt1C-Start]
	
	xchg	ax,[ds:1Ch*4]	;put new int 1Ch
	
	mov	[cs:bx],ax	;save old int 1Ch
	mov	ax,cs
	
	xchg	ax,[ds:1Ch*4+2]
	
	mov	[cs:bx+2],ax
	sti

	push	cs
	pop	ds
	lea	si,[bp+msgInstall-Start]
	
	mov	bx,OutStr
	int	OS_INT
	
	lea	ax,[bp+NewInt1C-Start]
	call	ShowWord
	
	
	mov al,'-'
	call localOutChar
	
	lea	ax,[bp+LastByte-Start]
	call	ShowWord
	
	mov	bx,NewLine
	int	OS_INT
		
	mov bx, GetMem 	;Alocate diferent page so that we wont be over writen
	int OS_INT
	
	retf
	

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
ShowWord:
	call	ShWo2
ShWo2:	xchg	al,ah

;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

ShowByte:
	call	ShBy1
ShBy1:	rol	al,4
	push	ax
	and	al,0Fh
	cmp	al,0Ah		;If al < 10, set CF = 1
	sbb	al,69h		;0-9: 96h..9Fh, A-F: A1h..A6h
	das			;0-9: subtr. 66h -> 30h..39h,
	call localOutChar	;A-F: subtr. 60h -> 41h..46h
	
	pop	ax
	ret

msgInstall:
	db	'Clock installed in ',0
	db	'move: M0 DF FF20  ->  GFF20 '

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; TSR routine don't overwrite!
NewInt1C:
	pusha
	push	es

	cld			;clear direction flag
	xor	ax,ax
	mov	es,ax
	mov	ax,VGAColor
	cmp	byte [es:0449h],07h
	jnz	NoMono
	mov	ax,VGAMono
NoMono:
	mov	es,ax
	mov	di,(Line-1)*160+(Column-1)*2	;screen offset

	mov	ah,02h
	int	1Ah		;get RTC value (HH-MM-SS in BCD !!!)
	mov	ah,Color
	mov	al,ch		;hours
	call	SetByte
	mov	al,Separator
	stosw
	mov	al,cl		;minutes
	call	SetByte
	mov	al,Separator
	stosw
	mov	al,dh		;seconds
	call	SetByte

	pop	es
	popa

	jmp	0000h:0000h	;jump to old int 1Ch handler
	
	OldInt1C	equ $-4

SetByte:
	push	ax
	shr	al,4		;get high nibble
	or	al,30h
	stosw
	pop	ax
	and	al,0Fh		;get low nibble
	or	al,30h
	stosw
LastByte:
	ret

	
localOutChar:
	; al = char to write

	push	bx
	mov	bx,0007h	;display page 0 with text attribute
	mov	ah,0Eh 		;BIOS - teletype char
	int	10h
	pop	bx
	ret
	
	
	
	
	
TIMEs 510-($-$$) db 0
dw 0xAA55