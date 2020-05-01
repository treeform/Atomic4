;
; Atomic Os - smallest fullfunctioning os 
;	4th relise
;	By Andre' Hauck
;			June 28, 2004
;


;	This is a os has all functions needed for regular user work
;	if they dont mind all dealing with 512 byte pages. 
;	You can load filEs
;	You can save filEs
;	You can run programs
;	
;	Each file has a number and is 512 bytes long
;	Which is kind of cheating since they are realy sectors
;		
;	The momory maneger uses 512 byte pages 
;		(Nobady realy need a bigger data structiure 
;		 becouse thats all that will fit in a file)
;
;
;	Thanks Asko from finland for writing for the OS 
;	a debuger and significunly optimizing the Atomic
;	sence then a memor maneger has been added

;	this is just a 16 bit os
bits 16

;where we load and edit files
;SectorBuffer	equ 0x8400 ;
SectorBuffer	equ 0h ;

;where the commands go
CommandBuffer   equ 300h 

;where the table of memory usige goes
MemTable 		equ 500h ; 1024 long

;this is a boot sector
org 00h			;bootsector start in 0:7C00h



;======================================================
;
;	Os init module 
;
;=======================================================
	


jmp	0x07C0:Start	; need to set cs (or 2*push & retf)

	
Start:	

	xor	ax, ax		;ax=0
	mov	ds, ax		;ds=0
	
	
 	mov	word [21h*4], IntHandler	; on in 21h call
	mov	word [21h*4+2], cs			; 7c0:IntHadle

	mov	ax, 0x07C0	; where we are
	mov	ds, ax		; make shure all are set to that
	mov es, ax		; es is normal wile we clean MemoryTabe (later it points to alocated memory)
 	mov	ss, ax		; statck sigemnt
 	mov	sp, 0x83FE	; set up stack 07C0h:83FEh = 0:FFFEh
 					; just before the "listed" memry
 					; Memory blow that is unlisted and so cannot be freed or alocated

	
 	;mov	si, StartMSG ; say "Atomic"
	;call OutStr

	;call ClearMem	; clear memory

	;ClearMem
	
	mov di,MemTable			; load di
	mov cx,1024-Unlisted	; load si
	mov al,Empty			; load with Empty
	repe stosb				; lets go!
		
	;ret
		
	
;======================================================
;
;	Os shell module 
;
;=======================================================
	
	
	
CmNew:	
	
	call GetMem		; alocate memory for the file buffer (es:0 - 200h big)	

	
AShell:

	cld				; direction = up the momory

	mov	si, Promp	; say ->
	call	OutStr

	mov	si, CommandBuffer	;input command
	mov	dx, 100h
	call	InStr

	mov	si, CommandTable	;set si to command table

NextCmd:
	
	mov	di, CommandBuffer	;set di to command

NewChar:
	
	lodsb					; AL = [SI]
	test	al, al			; if [si] = 0 then end of command 
	jz	FoundCmd			; in command table
	
	mov	ah, [di]			; load [di]
	inc	di					; next char in command
	or	ah, 0x20			; convert user input to lowercase
	cmp	al, ah				; compare user to command table
	jz	NewChar
	

		
LoopTo0:					; ok not this command 
	lodsb					; so now we need to go the end
	test al, al				; of command in the command table
	jnz	LoopTo0				; marked by 0
	
	
	add	si, byte 2			; skip over subroutine word
	cmp	si, CommandTableEnd	; is it over?
	
	jb	NextCmd				; no? - good, check next command 
	

;	mov si,BadCommand
;	call OutStr
	
	jmp	AShell				; yes? - done, get new command

FoundCmd:
	call word [si]			; call the table of commands
	jmp	AShell				; after we jmp back

	
	
;---------------------------------------------------------	
; 
;	command struct
;	'0' after the name and beffore function address
;
%macro cmd 2
	db %1		; name
	db 0		; turminating 0
	dw %2		; address
%endm
;
; 	Command table
;		Name and addres of command
;
CommandTable:
	cmd "load",	 	CmLoad			;load Sector#
	cmd "save", 	CmSave			;save Sector#
	cmd "edit", 	CmEdit			;edit
	cmd "run", 		CmRun			;run
	cmd "type", 	CmType			;type
	cmd "mem", 		CmMem			;mem
	cmd "new", 		CmNew			;new
	cmd "up", 		CmUpMem			;up
CommandTableEnd:


; little date area
;Promp	db 0x0D, 0x0A, "->", 0	; os promp
;StartMSG db 'Atomic',0				; os name
Promp:
StartMSG:
db 0x0D, 0x0A,"AtomicOS>",0

;BadCommand db 0x0D, 0x0A,"bad!",0

;============================================================
; Loads file into the current mempory page

CmLoad:
	mov	ah,2					;int 13h read 
	jmp short DoSomeSectorIO	;part of CmSave
	

;============================================================
; Saves the current mempory page to a file
	   
CmSave:
	mov	ah,3			; int 13h write instruction

DoSomeSectorIO:			; 
	
	push ax				; presureve ax
	call	InNum		; Figure out what that number is
	pop ax				; load ax
	
	mov	di,0			; dump it ad es:0

	;jmp	DiskIO  		; cool right under us
		
	;ret					; thats it

	
;=======================================================
; moved to here int order to save space of 3 bytes
; Disk I/O
; es:di -> buffer
; cx = sector number
; ah = function (2 = read, 3 = write)
;

DiskIO:

	; there is 18 sectors on a sylinder on 1.44M floppy

	push	ax		; save function number
	xchg	ax,cx	; sector number
	mov	cx,18
	xor	dx,dx
	div	cx
	inc	dl
	mov	cl,dl		; sector number (1..18)
	xor	dh,dh
	shr	al,1
	rcl	dh,1		; head number (0..1)
	mov	ch,al		; track number (0..79)

	pop	ax		; ? function.
	mov	al,01		; sectors to read.

	; ch = cylinder.
	; cl = sector.
	; dh = head.

	mov	bx,di

	mov	dl,0	;,[BootDisk]		     ;disk #
		
	; read or write!

	int	13h

	
	ret	;Now we return

;============================================================
; Edits the current mempory page

CmEdit:
	
	push es				; make ds be es too
    pop ds				; so that we can acsess 
    					; the curent mempry page
    					; just like data sigemnt
	
	mov	si,0			; es:0
	mov	dx,512			; all the area
	call InStr			; input!

	jmp short RestoreDSAndRet	

;	push cs
;   pop ds
	
;	ret


;============================================================
; Type Types the current momory page


CmType:

   push es				; make ds be es too
    pop ds				; so that we can acsess 
    					; the curent mempry page
    					; just like data sigemnt
	
	xor si,si			; es:0	
	Call OutStr			; just regular print
	
RestoreDSAndRet:
	
	push cs				; make data sigement where it was 
    pop ds				; before 
    
	
ret
;============================================================
; Mem displays where are the pages that are usde (1st page is at 10000h mark)


CmMem:
    

       
	Mov si,MemTable			; addres of table	
	mov cx,1024-Unlisted	; number of memory pages
		
	;jmp OutBuf				; just regular buffer dump
	
;ret
							
 OutBuf:   ;now resides incorectly in the command module section
; 			in oreder to save space

 	; ds:si -> string to write
 	; cx = char count

 .NextChar:
 	lodsb			; load next character
 	call	OutChar
 	loop	.NextChar

 	ret	;Now we return


CmRun:

	
	mov [far_ptr+2],es	; Set the far ptr addres to it
	xor ax,ax
	mov [far_ptr],ax	; Set the far ptr addres to it
	

	push es				; make ds be es too
    pop ds				; so that we can acsess 
    					; the curent mempry page
    					; just like data sigemnt
	  

	cmp	[510], word 0xAA55	; is it a vilid atomic program
	jne	RestoreDSAndRet		; no?
		
	call far [cs:far_ptr] 	; then go!
		
 jmp short RestoreDSAndRet	

   
 
CmUpMem:

	
	mov si,es				; load es into si
	shr si,5				; div si by 512( page size )
	sub si,Unlisted			; - the unlisted memory

	
.Next:	
	
	inc si					; si++
	
	cmp si, 1024-Unlisted	; are we though the page table?
	jnae .NotOver			; NO? continue
		xor si,si			; Yes? make si 0
	.NotOver:
	
	cmp byte [si+MemTable], Empty	; is it marked empy? ( Used marker may chage to reflect what is stored there )
	je .Next						; then continue loop
	
		
.GotIt						; yes found?

jmp MakeSIintoES			; do the oposite of wat was done at the top
	
	;add si,Unlisted 
	;shl si,5
	;mov es,si
	
;ret







;======================================================
;
;	Os kernal module 
;
;	Interupt
;	Disk I/O
;	Consol I/O and format
;	Memry manegment
;
;=======================================================

IntHandler:
	cmp	bx, 9
	ja	int21_ret
	add	bx,bx
	call	word [cs:bx+IntTable]
int21_ret:
	iret

IntTable:	; function list order MUST be same as function numbers !!!

	dw	DiskIO,OutChar,OutStr,OutBuf,InStr,NewLine,OutNum,InNum,GetMem,FreeMem



	
;=======================================================

InChar:
	; return: al = char

	;no multitask 
	;mov	ah,0		; wait for keypress
	;int	16h
	
;.Again:
	;multitiask
;	mov ah,01
;	int 16h
;	jnz HaveKey 
	
	;int 100h			;multitask
;	jmp short .Again
	
HaveKey:
	
	mov	ah,0		; wait for keypress
	int	16h
		
	ret
	
;=======================================================	
	
	InStr:
	; ds:si -> buffer 
	; dx size

	xor	bx,bx		; clear char counter
	
	
.InStrChar:
	
	call InChar		; get key
	cmp	al,13		; 'return' pressed?
	je	.InStrExit
	
	
	cmp	al,8		; 'backspace' pressed?
	je	.InStrBackspace
	
	cmp	bx,dx		; buffer is full?
	je	.InStrChar	; if so wait for 'backspace' or 'return'...
	
	mov	[bx+si],al  ; move character into the string
	inc	bx			; advace along the buffer

.InStrPrint:
	call	OutChar	; print key
	jmp	.InStrChar


	
		
.InStrBackspace:
	
	cmp	bx,0
	je	InStr		; if bx =0 nothing to remove

	dec bx
	
	call	OutChar
	mov	al,20h	; space
	call	OutChar
	mov	al,08h		; backspace
	
	jmp	.InStrPrint	

		
	
.InStrExit:
	mov	[bx+si],byte 0		; input string must be
							; terminate by NULL
								
	;call	NewLine			; right under us!
	;ret

;--------------------------------------------------

NewLine:
	
	mov	al,0Dh
	call	OutChar
	mov	al,0Ah
	;call	OutChar			; right under us!
	;ret	;Now we return


;---------------------------------------------------------

OutChar:
	; al = char to write

	push bx
	mov	bx,0007h 	; display page 0
	mov	ah,0x0E 	; BIOS teletype
	int	0x10		; invoke BIOS
	pop	bx
	ret	;Now we return

;---------------------------------------------------------


OutStrCall:
	call	OutChar

OutStr:
	; ds:si -> ASCIIZ string to write

	cld
OutStrChar:
	lodsb 			;load next character
	or	al,al		;test for NULL character
	jnz	OutStrCall 	;NULL ends the message
OutStrEnd:
	ret

;---------------------------------------------------------
;
; OutBuf:   now resides incorectly in the command module section
; 			in oreder to save space
; 	; ds:si -> string to write
; 	; cx = char count

; .NextChar:
; 	lodsb			; load next character
; 	call	OutChar
; 	loop	.NextChar

; 	ret	;Now we return


;---------------------------------------------------------

OutNum:
	; ax = number to write

	xor	cx,cx		; counter
	mov	bx,10		; 10 - divider.

PushLoop:
	
	xor	 dx,dx		; dx=0
	div	 bx			; Ax = Dx:Ax / Bx   (DX=remainder).
	push dx			; save reminder
	inc	 cx			; number of characters  
	test ax,ax		; did we mole the number down? 

	jnz	PushLoop	; no? loop

PopLoop:
	
	pop	ax			; get number 
	add	al,'0'		; convert to ASCII code.
	call	OutChar	; show the character

	loop	PopLoop ; more? loop

	ret	;Now we return

;---------------------------------------------------------

InNum:
	; ds:di -> ASCII number
	; return: cx = number

	xor	cx, cx		; clear cx

NextDigit:
					
	mov	al,[di]		; get char from buferr into AL:
	inc	di			; inscrement pointer
	
	cmp	al,' '		; if it is a leading space by pass
	
	je	NextDigit	; loop

; allow only digits:

	sub	al, '0' 	; convert from ASCII code:
	cmp	al, 9
	ja	StopInput

; Ok only digits here: 

	xchg ax,cx 		; multiply CX by 10 (first time the result is zero)
	mov	dx,10		; ten
	mul	dx			; Dx:Ax = Ax*10
	
	mov	ch,0		; strip off high bits
	add	cx,ax		; add AX to CX:

	jmp	NextDigit

StopInput:

	ret	; Now we return


		
;---------------------------------------------------------
; memory manegment

; all memory is devided into pages, each page is 512
; bytes so each page fits a file or a program.  With
; one megabyte there is total of 2048 pages. Each page
; is eather free or in use and it is marked in the 
; MemTable wich has a byte for each page (I was going 
; to do a bitmap but then i have a problem of fiting it 
; into the 20 bytes i got left.  
; 
;

Empty equ "_"			; z
Used  equ 'U'		; may change in newer versions to reflect wat is stored there



Unlisted equ 128	; lower memory area of the OS is unlsited

;---------------------------------------------------------



;---------------------------------------------------------
; gets a free page 512 bytes at es:0
; does not free current page
;

GetMem 

	xor si,si	; make 0 we will add 1 soon
	
.Next:	
	
	inc si								; Next
	cmp byte [cs:si+MemTable], Empty	; is it empty?
	jne .Next							; ?mo jmp
	

.GotIt									; Ok it seems we found it
	
	mov [cs:si+MemTable], byte Used		; mark it used
		
	
MakeSIintoES:	

	add si, Unlisted					; make
	shl si,5							; si into 
	mov es,si							; es - the current page
		
ret

;---------------------------------------------------------

FreeMem

	mov si,es									; just decode es
	shr si,5									; into si
	mov [cs:si+MemTable-Unlisted], byte Empty 	; and free!
	
ret

;MemTable:

; little about the memory layout 
; 
; page 1:	IntVectors      (0   - 200)
; page 2:	IntVectors		(200 - 400)
; page 3:	BiosData 		(400 - 600)
; page 4:	Bios            (600 - 800)
; page 5-128:	Atomic		(800 -10000)
; page 128-1024  user area  (1000 - 80000)
; page 1024 - ? Vido , Bios and who knows what else (80000 - 1M) 
; yet i know i can get more ram over this way :) 


TIMEs 510-($-$$) db 0
dw 0xAA55

; os data area:

far_ptr:



;we are now in sector 1 - it stores the help file

db "Hi this is Atomic Os the most functinal/size ratio I have seen",0x0D, 0x0A
db "The entire os fits on a 512 byte boot sector",0x0D, 0x0A

db "Load - read a file by number",0x0D, 0x0A
db "Save - read a file by number ",0x0D, 0x0A
db "Edit - edits the loaded file ",0x0D, 0x0A
db "Type - display the file (after load or Edit)",0x0D, 0x0A
db "New  - alocate new memory",0x0D, 0x0A
db "Mem  - show memory",0x0D, 0x0A
db "Up   - Select next used memory block",0x0D, 0x0A

db "Logical unit in Atomic OS is a page and it is 512 bytes.  Every thing is exactly one page.  So anyfile and program is a page.", 0x0D, 0x0A

db 0


TIMEs 1022-($-$$) db 0
dw 0x0
