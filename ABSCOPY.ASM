;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; Program to copy file to absolute sector into A:
;
; Usage:
;   AbsCopy [options] File SectorNo
;
; Options:
;   /p = keep old disk parameters
;
; Notes:
;   file name can't start with number
;   don't use /p options if you write whole disk image
;
; Compile:
;   nasm -f bin -oabscopy.com abscopy.asm
;
; Version history:
;   634 - 23.Dec.2003 by ATV
;   744 - 4.Jun.2004 by ATV
;	  Int 13/ah=08 removed, now get head/sector count via boot sector
;	  Display progress
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
[bits 16]
[org 0x0100]
[segment .text]

CMDLINE		equ 0x0080
BYTESPERSECTOR	equ 512

Start:	cld
	push	cs
	pop	es
	push	cs
	pop	ds
	mov	di,File_Name
	mov	si,CMDLINE		;start of cmd line
	xor	ax,ax
	mov	[Count],ax
	mov	[ErrSecs],ax
	dec	ax
	mov	[Sector_No],ax
GetChar:inc	si
	mov	ax,[si] 		;get char
	cmp	al," "
	jbe	ChkEnd			;skip blanks and tabs
	cmp	al,"/"
	jz	GetParam
	cmp	al,"-"
	jz	GetParam
	cmp	al,"9"
	ja	GetName
	cmp	al,"0"
	jb	GetName
	call	GetNumber
	mov	[Sector_No],ax
	jmp	short ChkEnd
GetParam:
	lodsb
	and	al,0xDF			;next char to upper case
	mov	[param_flag],al
	jmp	short ChkEnd
GetName:
	cmp	di,File_Name
	jnz	ChkEnd
GeNa10:	movsb
	cmp	byte [si]," "		;check next char
	ja	GeNa10
	mov	byte [di],0
ChkEnd: cmp	byte [si],0x0D		;is end of line
	jnz	GetChar
	cmp	word [Sector_No],byte -1
	mov	dx,txtUsage
	jz	ErrExit
	cmp	di,File_Name
	jz	ErrExit

	mov	di,5			;five retries for error
ReBo10:	mov	bx,BootSector		;Read boot sector
	mov	cx,1			;ch=cylinder, cl=sector
	xor	dx,dx			;dh=head, dl=drive
	mov	ax,0x0201		;BIOS - Read one sector
	int	0x13
	jnc	ReBo20			;test for read error
	xor	ax,ax			;BIOS - Reset disk
	int	0x13
	dec	di			;decrement error counter
	jnz	ReBo10			;attempt to read again
	mov	dx,txtBoot		;Can't read bootsector, exit to DOS
	jmp	short ErrExit
ReBo20:	cmp	word [BootSector+0x01FE],0xAA55	;Is bootsector id?
	jnz	ReBo30			;if not use default 1.44M values
	mov	ax,[BootSector+0x1A]	;NumHeads
	dec	ax
	cmp	ax,2			;accept if heads is 1 or 2
	jnb	ReBo30
	mov	si,BootSector+0x0B
	mov	di,Floppy_Param
	mov	cx,0x0010
	rep	movsw			;copy floppy parameters
ReBo30:	call	CopyFile
	jc	ErrExit
	mov	ax,[Count]
	call	ShowNumber
	mov	dx,txtSecs
	mov	ah,0x09			;DOS - Write string
	int	0x21
	mov	dl,0x0A
	mov	ah,0x02			;DOS - Write char
	int	0x21
	mov	ax,[ErrSecs]
	test	ax,ax			;ERRORLEVEL=0
	jz	TheEnd
	call	ShowNumber
	mov	dx,txtBad
ErrExit:
	mov	ah,0x09			;DOS - Write string
	int	0x21
	mov	al,0x01			;ERRORLEVEL=1
TheEnd:
	mov	ah,0x4C			;DOS - Terminate program
	int	0x21
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; Procedure CopyFile
; return: CF set on error (dx=error message)
CopyFile:
	mov	dx,File_Name
	mov	ax,0x3D00		;DOS - Open file
	int	0x21
	mov	dx,txtOpen
	jc	Copy_exit
	xchg	ax,bx
NextBlock:
	mov	dx,Buffer
	mov	cx,512*64		;32k buffer
	mov	ah,0x3F			;DOS - Read from file
	int	0x21
	mov	dx,txtRead
	jc	CloseFErr
	test	ax,ax
	jz	CloseF
	add	ax,BYTESPERSECTOR-1
	mov	cl,9
	shr	ax,cl
	mov	[SecCount],ax
	push	bx			;save file handle
	mov	bx,Buffer
NextSector:
	mov	ax,[Sector_No]
	test	ax,ax			;is boot sector?
	jnz	Not_BootSec
	cmp	byte [param_flag],"P"	;keep old disk parameters?
	jnz	Not_BootSec
	mov	si,Floppy_Param
	mov	di,Buffer+0x0B
	mov	cx,0x0010
	rep	movsw			;copy old disk parameters
Not_BootSec:
	mov	cx,1
	call	WriteSectors
	jnc	WrOk
	inc	word [ErrSecs]
	jmp	short WrErr
WrOk:	inc	word [Count]
WrErr:	inc	word [Sector_No]
	dec	word [SecCount]
	jnz	NextSector
	mov	ax,[Count]
	call	ShowNumber
	mov	dx,txtSecs
	mov	ah,0x09			;DOS - Write string
	int	0x21
	pop	bx			;restore file handle
	jmp	short NextBlock
CloseF:	clc
CloseFErr:
	pushf
	mov	ah,0x3E			;DOS - Close file
	int	0x21
	popf
Copy_exit:
	ret
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; Procedure ReadSectors
; reads 'cx' sectors from disk starting at sector 'ax'
; into memory location 'es:bx'
ReadSectors:
	mov	di,5			;five retries for error
ReSe10: push	ax
	push	bx
	push	cx
	call	LBACHS
	mov	ax,0x0201		;BIOS - Read one sector
	int	0x13
	jnc	ReSe20			;test for read error
	xor	ax,ax			;BIOS - Reset disk
	int	0x13
	pop	cx
	pop	bx
	pop	ax
	dec	di			;decrement error counter
	jnz	ReSe10			;attempt to read again
	add	bx,BYTESPERSECTOR	;queue next buffer
	inc	ax			;queue next sector
	stc
	ret
ReSe20: pop	cx
	pop	bx
	pop	ax
	add	bx,BYTESPERSECTOR	;queue next buffer
	inc	ax			;queue next sector
	loop	ReadSectors		;read next sector
	clc
	ret
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; Procedure WriteSectors
; writes 'cx' sectors from memory location 'es:bx'
; into disk starting at sector 'ax'
WriteSectors:
	mov	di,5			;five retries for error
WrSe10: push	ax
	push	bx
	push	cx
	call	LBACHS
	mov	ax,0x0301		;BIOS - Write one sector
	int	0x13
	jnc	WrSe20			;test for write error
	xor	ax,ax			;BIOS - Reset disk
	int	0x13
	pop	cx
	pop	bx
	pop	ax
	dec	di			;decrement error counter
	jnz	WrSe10			;attempt to write again
	add	bx,BYTESPERSECTOR	;queue next buffer
	inc	ax			;queue next sector
	stc
	ret
WrSe20: pop	cx
	pop	bx
	pop	ax
	add	bx,BYTESPERSECTOR	;queue next buffer
	inc	ax			;queue next sector
	loop	WriteSectors		;write next sector
	clc
	ret
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; Procedure LBACHS
; convert 'ax' LBA addressing scheme to CHS addressing scheme
; cl = (logical sector / sectors per track) + 1
; dh = (logical sector / sectors per track) MOD number of heads
; ch = logical sector / (sectors per track * number of heads)
; dl = drive
LBACHS:
	xor	dx,dx			;calculate sector
	div	word [SectorsPerTrack]
	mov	cx,dx
	xor	dx,dx			;calculate head
	div	word [NumHeads]
	mov	dh,dl
	xchg	al,ah
	ror	al,1			;roll track bits 8 and 9
	ror	al,1
	and	al,0xC0			;mask off extra bits
	inc	cx			;adjust for sector 0
	or	cx,ax			;ch=track, cl=sector+2hi track bits
	mov	dl,0			;drive A:
	ret
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
GetNumber:
	xor	bx,bx
GeNu20: mov	al,[si]
	sub	al,'0'
	cmp	al,9
	ja	GeNu90
	inc	si
	cbw
	xchg	ax,bx
	mov	dx,0x000A
ten	equ	$-2
	mul	dx
	add	bx,ax
	jmp	short GeNu20
GeNu90: xchg	ax,bx
	ret
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
ShowNumber:
	xor	cx,cx
ShNu10: xor	dx,dx
	div	word [ten]
	push	dx
	inc	cx
	test	ax,ax
	jnz	ShNu10
ShNu20: pop	dx
	add	dl,'0'
	mov	ah,0x02			;DOS - Write char
	int	0x21
	loop	ShNu20
	ret
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
txtUsage	db "Copy file to absolute sector into A:",0x0D,0x0A
		db "Usage:",0x0D,0x0A
		db "  AbsCopy [options] File SectorNo",0x0D,0x0A
		db "Options: /p = keep old disk parameters",0x0D,0x0A,"$"
txtBoot 	db 0x07,"Can't read boot sector",0x0D,0x0A,"$"
txtOpen 	db 0x07,"Can't open file",0x0D,0x0A,"$"
txtRead 	db 0x07,"Read error",0x0D,0x0A,"$"
txtSecs 	db " sectors written",0x0D,"$"
txtBad		db " write errors",0x0D,0x0A,"$"
param_flag	db 0			;nonzero if keep old disk parameters

Floppy_Param:				;Default 1.44M floppy values
BytesPerSector		dw 0x0200
SectorsPerCluster	db 0x01
ReservedSectors 	dw 0x0001
TotalFATs		db 0x02
MaxRootEntries		dw 0x00E0
TotalSectorsSmall	dw 0x0B40
MediaDescriptor 	db 0xF0
SectorsPerFAT		dw 0x0009
SectorsPerTrack 	dw 0x0012
NumHeads		dw 0x0002
HiddenSectors		dd 0x00000000
TotalSectorsLarge	dd 0x00000000
DriveNumber		db 0x00
Flags			db 0x00
Signature		db 0x29
VolumeID		dd 0xFFFFFFFF

[segment .bss]
SecCount	resw 1
Sector_No	resw 1
Count		resw 1
ErrSecs 	resw 1
File_Name	resb 256
BootSector	resb 512		;address of old boot sector
Buffer		resb 512*64		;address of read buffer
