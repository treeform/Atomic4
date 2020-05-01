
;		Attomic Calculator
; 
;		What Os can live without a calculator??
; 
;
; Insctuction set
;
; +, -, *, /, 

;Definitions for functions:
%include "Atomicos.inc"



org 0h





CalcStart:

	
CLoop:

	;mov [Number], word 0

	mov si,CPromp
	mov bx, OutStr
	int 21h
	
	mov si, Buffer
	mov dx, BufferSize
	mov bx, InStr
	int 21h
	
	mov di,Buffer
	
	cmp [di], byte 'x'
	jne .NoExit
		retf
	.NoExit:
			
	mov di,Buffer
	call InInt
	dec di
	mov [Number],cx

; 	mov ax,[Number]
; 	call OutInt
; 	
; 	jmp CLoop
; 	
		
NextNumber:	
	


 	cmp [di], byte "#"
 	je GetOldNumber

 	cmp [di], byte "+"
 	je AddNumber
	
	cmp [di], byte "-"
	je SubNumber
		
	cmp [di], byte "*"
	je MulNumber
	
	cmp [di], byte "/"
	je DivNumber
	
	mov bx,NewLine
	int 21h
	
	mov ax,[Number]
	call OutInt
	
	mov ax,[Number]
	mov [OldNumber],ax
	
	
	jmp CLoop

CPromp	db 0x0D, 0x0A, "C:", 0



GetOldNumber:

mov ax,[OldNumber]
mov [Number],ax

jmp NextNumber




; ADD ax,[#]	+
AddNumber:
inc di 

call InInt
add [Number],cx
dec di


jmp NextNumber


; SUB ax,[#]	-
SubNumber:
inc di 

call InInt
sub [Number],cx
dec di


jmp NextNumber


; MUL ax,[#]	*
MulNumber:
inc di 
call InInt
dec di
mov ax,cx
mul word [Number]
mov [Number],ax
jmp NextNumber


; DIV ax,[#]	
DivNumber:



inc di 
call InInt
dec di

mov ax,[Number]
mov bx,cx

cmp ax,0
je SayError

cmp bx,0
je SayError

cmp ax,bx
je GiveThemOne
jl GiveThemZero

div bx

mov [Number],ax

jmp NextNumber


GiveThemOne:
mov [Number], word 1
jmp NextNumber

GiveThemZero:
mov [Number], word 1
jmp NextNumber





SayError:
	mov si,Error
	
	mov bx,OutStr
	int 21h
	
jmp CLoop

Error db "Error"

Number dw 0
OldNumber dw 0


;=========================================================
;		Working with singed numbers
;		Atomic OS provides just unsind numbers support



; ;---------------------------------------------------------	 
; ;
; ;	OutInt
; ;
; ;	prints the number in Ax
; ;	
;  
;        
; OutInt:


; 		cmp ax,  0
; 		jge	.NoMinus
; 			
; 			push ax
; 	   	 	Mov     AL, '-'
;         	Mov     AH, 0Eh
;         	INT     10h    
; 			pop ax
;         	
;         	not ax
;         	add ax,1
;         	        	
; 		.NoMinus:



;         Mov     CX, 1			;flag to prevent printing 
;         						;zeros before number

;         Mov     BX, 10000 		;(result of "/ 10000" is 
;         						;always less or equal to 9).
;               					;2710h - divider.
;         						
;         Cmp     Ax, 0			;Ax is zero?
;         JZ      .PrintZero		;then print it

; .BeginPrint:

;         
;         Cmp     BX,0			;check divider 
;         JZ      .EndPrint		;(if zero go to end_print)

;         					
;         Cmp     CX, 0			;avoid printing zeros before number
;         Je      .Calc
;         						
;         Cmp     Ax, BX			;if Ax<BX then result of DIV
;         JB      .Skip			;will be zero
; .Calc:
;         Mov     CX, 0   		;set flag.
;         Mov     DX, 0
;         DIV     BX      		;Ax = DX:Ax / BX   (DX=remainder).
;    					
;         ADD     AL, 30h    		;convert to ASCII code.
;         Mov     AH, 0Eh			;print last digit
;         INT     10h				;AH is always ZERO, so it's ignored
;    
;         Mov     Ax, DX  		;get remainder from last div 
;         						;so we can get the next digit

; .Skip:
;        
;         PUSH    Ax				;calculate BX=BX/10
;         Mov     DX, 0			
;         Mov     Ax, BX
;         DIV     word [Divider]  	;Ax = DX:Ax / 10   (DX=remainder).
;         Mov     BX, Ax
;         POP     Ax

;         JMP     .BeginPrint
;         
; .PrintZero:
;   			
; 			
; 	    Mov     AL, '0'
;         Mov     AH, 0Eh
;         INT     10h      
;         
; .EndPrint:
; 		       
;         Ret ;Now we return
;         
; Divider             DW      10      ; used as divider.    



; ;---------------------------------------------------------	 
; ;
; ;	InInt
; ;
; ;	get the number pointed by di
; ;	and returns it in cx
; InInt:
; 	
; 	 
; 	

; 	Mov     CX, 0
;     mov [minus], byte "+"
;      

; .NextSpaceOrMinus:   
;        
; 	Mov al,[di]     ;get char from buffer into AL
; 	inc di			;atvance
;         
; 	Cmp al,' '		;is it a leading space?
; 	Je .NextSpaceOrMinus	;then 
; 	     
; 	Cmp al,'-'		;is it a -
; 	Je .MakeMinus	;then 
; 				
; 	dec di
; 	
; .NextDigit:
; 	
; 	Mov al,[di]     ;get char from buffer into AL
; 	inc di	
; 	
; 	;allow only digits:	
; 	
;     Cmp     AL, '0'			;stop if less
;     JNAE    .StopInput		
;         
;     Cmp     AL, '9'			;stop if more
;     JNBE    .StopInput		
;     
;     
;     ;multiply CX by 10 (first time the rEsult is zero)
;         
;     PUSH    Ax
;     Mov     Ax, CX
;     MUL     word [Divider]       ; DX:Ax = Ax*10
;     Mov     CX, Ax
;     POP     Ax

;     
;     
;     SUB     AL, 30h			;convert from ASCII code:

;     
;         
;     Mov     AH, 0			;add AL to CX
;     Mov     DX, CX      	;backup, in case the result 
;     						;will be too big.
;     ADD     CX, Ax
;    
;     JMP     .NextDigit

;     
;     
; .StopInput:   
; 	
; 		cmp [minus], byte "-"
; 		jne .HasNoMinus
; 			
; 			sub cx,1
; 			not cx
; 				
; 		.HasNoMinus:
; 		
; 		
; 		
; 		Ret ;Now we return

;   .MakeMinus:
;   		mov [minus], byte "-"
;   		jmp .NextDigit
;   
; minus db " "
;---------------------------------------------------------
;---------------------------------------------------------
OutInt:
	; ax = number to write

	push cx
	push dx
	
	
	cmp ax,  0
 		jge	.NoMinus
 			push ax
 	   	 	
 			mov     AL, '-'
         	mov 	bx,OutChar
         	INT     21h    
         	
         	pop ax
         	
         	not ax
         	add ax,1
         	        	
 	.NoMinus:
	
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
	mov bx,OutChar	; show the character
 	int 21h
	
	loop	PopLoop ; more? loop

	
	
	pop dx
	pop cx
	
	ret	;Now we return

;---------------------------------------------------------

InInt:
	; ds:di -> ASCII number
	; return: cx = number


	push ax
	
	xor	cx, cx		; clear cx
	mov bl,'+'
	
NextPre:
					
	mov	al,[di]		; get char from buferr into AL:
	inc	di			; inscrement pointer
	
	cmp	al,' '		; if it is a leading space by pass
	je	NextPre	; loop

	cmp al,'-'
	jne .NotNegative
		mov bl,'-'	
		je	NextPre	; loop
	.NotNegative:
	
	dec di
	
NextDigit:

	inc di

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


	cmp bl,'-'
	jne .NotNegative
		neg cx
	.NotNegative:
	
		
	pop ax
	
	
	ret	; Now we return


Buffer:
 BufferSize equ 510 - ($-$$)





TIMES 510-($-$$) DB 0			
DW 0xAA55	



;Little help file at the back!
	 

db "Hi, this is the Attomic Calculator for my Attomic Os",0x0D, 0x0A
db "I belive that an os is not an os till it has a calculator!",0x0D, 0x0A
db "So Here I give all that is neaded for fun calculating!",0x0D, 0x0A
db "use the +,-,/,* at your own risk!",0x0D,0x0A,0


TIMES 1024-($-$$) DB 0			


