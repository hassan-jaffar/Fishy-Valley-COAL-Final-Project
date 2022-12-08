; COAL PROJECT 
; Group Members:
;------------------------------
; Waleed Malik & Hassan Jaffar 
;------------------------------

[org 0x0100]

jmp main

restorebuffer: times 4000 db 0 ; space for 4000 bytes

oldisr:	dd 0 		; space for saving old isr 
locate: dw 3080		;initial location of fish
loca:	db 0			;used to save key press location
coinloc: dw 0x2720
cointick: dw 0,0
oldtsr: dd 0
ticks:  dw 0
fishloc: dw 0
score: dw 0
escapebtn: dw 0
hookflag: dw 0

randomNum dw 0   
g_ticks:      dw 0 
r_ticks:      dw 0 
seconds:    dw 0    


askname:	db 'Kindly Enter Your Name: $'				;'$' terminated string
maxlength: dw 80 ; maximum length of input
startgame: db 10, 13, 'Press enter to start the game $' ; greetings message
buffer: times 81 db 0 ; space for input string

greeting:		db 'Hello'				;'$' terminated string
welcome:		db 'Welcome to Fishy Hunt'				;'$' terminated string
rule:		db 'Use arrow keys to move the fish and collect coins.'				;'$' terminated string
rule1:		db 'Green coin has 10 points and Red coin has 50 points.'				;'$' terminated string
rule2:		db 'Collect as many coins as you can.'				;'$' terminated string
rule3:		db 'Avoid ships and base to survive.'				;'$' terminated string
developer:		db 'Developed by Hassan Jaffar (21L-5260) & Waleed Malik (21L-5248) as COAL Project'				;'$' terminated string
escapegame:		db 'Are you sure you want to quit? Yes(Y)/No(N) $'				;'$' terminated string
scorestr:		db 'Score: $'				;'$' terminated string
pressenter:		db 'Press Enter to Continue $'				;'$' terminated string

;-------------------------------------------------------------------
; keyboard interrupt service routine
;-------------------------------------------------------------------
kbisr:		push ax
			push es
			push bx
			push dx
			push di
			mov bx,[cs:locate]   ;location
			mov di,bx
			mov ax,0xb800
			mov es,ax
			mov dx, 0x3320   ;ascii	of blank water	0001 1100 0010 0000

			in al, 0x60 ; read a char from keyboard port
			
			cmp al, 0x01 ; has the esc key pressed.........0100 1011
			jne nextcmp
			mov word [cs:escapebtn], 1
			call saveScreen
			jmp moveahead


nextcmp:	cmp al, 0x4b ; has the left arrow pressed.........0100 1011
			jne nextcmp1 ; no, try next comparison
			;mov byte[loca], 1 ; move 2 points left
			sub bx, 2
			cmp bx, 2718
			je comeleft
			cmp bx, 2878
			je comeleft
			cmp bx, 3038
			je comeleft
			cmp bx, 3198
			je comeleft
			cmp bx, 3358
			je comeleft
			cmp bx, 3518
			je comeleft
			cmp bx, 3678
			je comeleft
			cmp bx, 3838
			je comeleft
			cmp bx, 3998
			je comeleft
			jmp moveahead
			;jmp nomatch ; leave interrupt routine

comeleft:	add bx, 160
			jmp moveahead
		
nextcmp1:	cmp al, 0x4d ; has the right arrow pressed......... 0100 1101
			jne nextcmp2 ; no, try next comparison
			;mov byte[loca], 2 ; move 2 points right
			add bx, 2
			cmp bx, 2880
			je comeright
			cmp bx, 3040
			je comeright
			cmp bx, 3200
			je comeright
			cmp bx, 3360
			je comeright
			cmp bx, 3520
			je comeright
			cmp bx, 3680
			je comeright
			cmp bx, 3840
			je comeright
			cmp bx, 4000
			je comeright
			jmp moveahead
			;jmp nomatch ; leave interrupt routine
			
comeright:	sub bx, 160
			jmp moveahead
		
nextcmp2:	cmp al, 0xcb ; has the left arrow released........1100 1011
			jne nextcmp3 ; no, try next comparison
			;mov byte[loca], 0 ; do nothing
			jmp moveahead ; leave interrupt routine 

nextcmp3:	cmp al, 0xcd ; has the right arrow released..... 1100 1101
			jne nextcmp4 ; no, chain to old ISR
			;mov byte[loca], 0 ; do nothing
			jmp moveahead ; leave interrupt routine
			
nextcmp4:	cmp al, 0x48 ; has the up arrow pressed.........0100 1000
			jne nextcmp5 ; no, try next comparison
			;mov byte[loca], 3; move up
			sub bx, 160
			cmp bx, 2720
			jnae notup
			jmp moveahead
			;jmp nomatch ; leave interrupt routine
			
	notup:	add bx, 160
			call sound
			jmp moveahead
		
nextcmp5:	cmp al, 0x50 ; has the down arrow pressed......... 0101 0000
			jne nextcmp6 ; no, try next comparison
			;mov byte[loca], 4 ; move down
			add bx, 160
			cmp bx, 4000
			jae notdown
			jmp moveahead			
			;jmp nomatch ; leave interrupt routine
			
notdown:	sub bx, 160
			call sound
			jmp moveahead
		
nextcmp6:	cmp al, 0xc8 ; has the up arrow released........1100 1000
			jne nextcmp7 ; no, try next comparison
			;mov byte[loca], 0 ; do nothing
			jmp moveahead ; leave interrupt routine 

nextcmp7:	cmp al, 0xd0 ; has the down arrow released..... 1101 0000
			jne nomatch ; no, chain to old ISR
			;mov byte[loca], 0 ; nothing
			jmp moveahead ; leave interrupt routine

moveahead:	cmp word [cs:escapebtn], 1
			je continue

			mov word[es:di],dx
			mov [cs:locate], bx	
			mov di,bx
			mov dx, 0x3c40   ;ascii		0001 1100 0011 1110
                        
			mov word[es:di],dx
			cmp di, [coinloc+0]
			je add10
			cmp di, [coinloc+2]
			je add50
			jmp nomatch
			
add10:		add word [cs:score], 10
			jmp nomatch
			
add50:		add word [cs:score], 50
			jmp nomatch
			
continue:

			call exitinterface
			
escloop:
			in al, 0x60
			
			cmp al, 0x15
			je unhook
			
			cmp al, 0x31
			jne escloop
			
			call restoreScreen
			mov word [cs:escapebtn], 0
			jmp nomatch
			
unhook:
		mov word [cs:hookflag], 1
		
			
			
		
nomatch:	call printscore
			mov al, 0x20
			out 0x20, al ; send EOI to PIC
			pop di
			pop dx
			pop bx
			pop es
			pop ax
			jmp far [cs:oldisr]				; call the original ISR
			iret ; return from interrupt
		
clrscr: 
		push es
		push ax
		push di

		mov ax, 0xb800
		mov es, ax 
		mov di, 0

nextloc:
		mov word [es:di], 0x0720 
		add di, 2 
		cmp di, 4000 
		jne nextloc

		pop di
		pop ax
		pop es

		ret
		
menuclrscr: 
		push es
		push ax
		push di

		mov ax, 0xb800
		mov es, ax 
		mov di, 0

nextloc1:
		mov word [es:di], 0x3320 ;0011 0011 0010 0000
		add di, 2 
		cmp di, 4000 
		jne nextloc1

		pop di
		pop ax
		pop es

		ret
		
		
saveScreen:	pusha
push ds
push es	


			mov cx, 4000 ; number of screen locations

					

			mov ax, 0xb800
			mov ds, ax ; ds = 0xb800

			push cs
			pop es
		
			mov si, 0
			mov di, restorebuffer

			cld ; set auto increment mode
			rep movsb ; save screen

			;[es:di] = [ds:si]
			pop es
			pop ds
			popa
			ret
			
restoreScreen:		pusha	
				push ds
				push es


			mov cx, 4000 ; number of screen locations

					

			mov ax, 0xb800
			mov es, ax ; ds = 0xb800

			push cs
			pop ds
		
			mov si, restorebuffer
			mov di, 0

			cld ; set auto increment mode
			rep movsb ; save screen

			;[es:di] = [ds:si]
			pop es
			pop ds
			popa
			ret

delay:
		push cx
		mov cx, 0xFFFF

loop1: 
		loop loop1
		mov cx, 0xFFFF

loop2: 
		loop loop2
		pop cx

		ret

		
fish:
		push ax
		push bx
		push es
		push di
		
 		mov bx,[locate]   ;location
		mov di,bx
		mov ax,0xb800
		mov es,ax
                        
		mov ax, 0x3340   ;ascii		0001 1100 0011 1110
                        
		mov word[es:di],ax		
		
		pop di
		pop es
		pop bx
		pop ax

		ret

MoveLeft:
		pusha
		push es
		push ds

		mov ax,0xb800
		mov es,ax
		mov ds,ax

		mov di,160
		mov bx,8
		mov si,162
		cld
loop3:
		mov cx,79
		mov ax,[es:di]
		cld
		rep movsw
		mov word[es:di],ax
		add si,2
		add di,2
		dec bx
		jnz loop3

		call delay

		pop ds
		pop es
		popa
		ret

MoveRight:
		pusha
		push es
		push ds

		mov ax,0xb800
		mov es,ax
		mov ds,ax

		mov di,2718
		mov bx,8
		mov si,2716
		std
loop4:
		mov cx,79
		mov ax,[es:di]
		std
		rep movsw
		mov word[es:di],ax
		sub si,2
		sub di,2
		dec bx
		jnz loop4

		call delay
		
		pop ds
		pop es
		popa
		ret
	

removecoin: push bp
			mov bp, sp
			pusha
			push es
			
			mov ax, 0xb800
			mov es, ax
			mov di, [bp+4]
			mov word [es:di], 0x3020
			
			pop es
			popa
			pop bp
			ret 2
			




	
Sky: 
		push es
		push ax
		push di

		mov ax, 0xb800
		mov es, ax
		mov di, 0 

next_sky: 
		mov word [es:di], 0x0bdb
		add di, 2 
		cmp di, 1280 
		jne next_sky 

		mov di,1280

next_grass:
		mov word [es:di], 0x2240
		add di, 2
		cmp di, 1440 
		jne next_grass

;Printing the Sun

		mov di,280
		mov word [es:di], 0xeedb
		add di,2

		mov word [es:di], 0xeedb
		add di,2

		mov word [es:di], 0xeedb
		add di,156

		mov word [es:di], 0xeedb
		add di,2

		mov word [es:di], 0xeedb
		add di,2

		mov word [es:di], 0xeedb

		pop di
		pop ax
		pop es

		ret

printShip:                            
		push es
                push ax
                push si
                push di
                push dx
                push cx

                mov ax,0xb800
                mov es,ax
                mov di,bx
                mov ax,0x08db
				
				
	          sub di,320;
			  add di,dx
			  mov si,2
			  mov cx,2
	   printhead:
	          mov word[es:di],0x0fdb
			  add di,2
			  sub si,1
			  jz updatehead
			  jmp printhead
	updatehead:
            	sub cx,1
				jz boatini
				add di,160
				mov si,4
				sub di,3
				sub di,3
				jmp printhead
				
				
				
				boatini:
				mov di,bx
                mov si,3
                mov cx,dx   ;push from main
				
				
				
                                        
printlines:
                 cmp cx,0
                je nextupdate
                mov word[es:di],ax
                add di,2
                sub cx,1
                jmp printlines

nextupdate:
		sub si,1
                jz shipExit
                add di,160
                sub di,dx
                sub di,dx
                add di,2
                sub dx,2   ;4
                mov cx,dx
               	jmp printlines
                                        
shipExit:
		pop cx
                pop dx
                pop di
                pop si
                pop ax
                pop es

                ret

Boats:
		push bx
       		push dx
       		push cx


            
          	mov bx,1878
       		mov dx,20
           	call printShip

       		mov bx,1768
       		mov dx,20
            	call printShip

        	mov bx,2138
       		mov dx,26
            	call printShip
                
   		pop cx   
  		pop dx
  		pop bx

		ret
		
updatecoins:
		pusha
		
		
		
		popa
		ret

printblock: 		

		push es
		push bx
		push cx
		push ax
		push dx
                push si

		mov di,bx
		mov ax,0xb800
		mov es,ax
        mov si,4
		mov ax, dx
                        
print_l:
		mov word[es:di],ax
        add di,2
        sub si,1
        jz nxt_up
		jmp print_l

nxt_up:
		sub cx,1
                jz exit
                mov si,4
                add di,152
                jmp print_l

exit:
		pop si
		pop dx
                pop ax
		pop cx
		pop bx
		pop es

                ret                        



Sea: 
		push es
    		push ax
    		push di

		mov ax, 0xb800
		mov es, ax 
		mov di, 1440 

next_sea:
		mov word [es:di], 0x33db 
		add di, 2  
		cmp di, 4000 
		jne next_sea 

		pop di
		pop ax
		pop es

		ret


Buildings:
		push bx
		push cx
		push dx

		mov dx, 0x0ddb   ;ascii
 		mov bx,654   ;location
          	mov cx,5   ;length
		call printblock

		mov dx, 0x0ddb	
        	mov bx,340
          	mov cx,7
          	call printblock

		mov dx, 0x0ddb
 		mov bx,508
          	mov cx,6
		call printblock

		mov dx, 0x0ddb
 		mov bx,684
          	mov cx,5
		call printblock

		mov dx, 0x0ddb
 		mov bx,688
          	mov cx,5
		call printblock

		mov dx, 0x0ddb
 		mov bx,690
          	mov cx,5
		call printblock

		mov dx, 0x0ddb
 		mov bx,736
          	mov cx,6
		call printblock

		mov dx, 0x0ddb
 		mov bx,568
          	mov cx,7
		call printblock

		mov dx, 0x0ddb
 		mov bx,880
          	mov cx,5
		call printblock

		mov dx, 0x0ddb
 		mov bx,756
          	mov cx,6
		call printblock

		mov dx, 0x0ddb
 		mov bx,780
          	mov cx,6
		call printblock

		mov dx, 0x0ddb
 		mov bx,628
          	mov cx,7
		call printblock


		pop dx         
		pop cx
		pop bx

		ret
		
			
interface:	pusha

			call clrscr
			mov dx, askname	 						; ds:dx points to '$' terminated string
			mov ah, 9 								; service 9 –  WRITE STRING TO STANDARD OUTPUT
			int 0x21 								; dos services
			mov cx, [maxlength] ; load maximum length in cx
			mov si, buffer ; point si to start of buffer
			nextchar: mov ah, 1 ; service 1 – read character
			int 0x21 ; dos services
			cmp al, 13 ; is enter pressed
			je exit2 ; yes, leave input
			mov [si], al ; no, save this character
			inc si ; increment buffer pointer
			loop nextchar ; repeat for next input char
			exit2: 

			call menuclrscr

		mov ah, 0x13		; service 13 - print string
		
		mov al, 1			; subservice 01 – update cursor 
 
		mov bh, 0			; output on page 0
		
		mov bl, 00110000B	; normal attrib
		mov cx, 5		; length of string
		mov dx, 0x0200		; row 10 column 10
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, greeting		; bp = offset of string
		
		INT 0x10			; call BIOS video service

		mov ah, 0x13		; service 13 - print string
		
		mov al, 1			; subservice 01 – update cursor 
 
		mov bh, 0			; output on page 0
		
		mov bl, 00110000B	; normal attrib
		mov cx, 20		; length of string
		mov dx, 0x0206		; row 10 column 10
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, buffer		; bp = offset of string
		
		INT 0x10			; call BIOS video service

		mov ah, 0x13		; service 13 - print string
		
		mov al, 1			; subservice 01 – update cursor 
 
		mov bh, 0			; output on page 0
		
		mov bl, 00110000B	; normal attrib
		mov cx, 21		; length of string
		mov dx, 0x0400		; row 10 column 10
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, welcome		; bp = offset of string
		
		INT 0x10			; call BIOS video service

		mov ah, 0x13		; service 13 - print string
		
		mov al, 1			; subservice 01 – update cursor 
 
		mov bh, 0			; output on page 0
		
		mov bl, 00110000B	; normal attrib
		mov cx, 50		; length of string
		mov dx, 0x0800		; row 10 column 10
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, rule		; bp = offset of string
		
		INT 0x10			; call BIOS video service

		mov ah, 0x13		; service 13 - print string
		
		mov al, 1			; subservice 01 – update cursor 
 
		mov bh, 0			; output on page 0
		
		mov bl, 00110000B	; normal attrib
		mov cx, 52		; length of string
		mov dx, 0x0a00		; row 10 column 10
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, rule1		; bp = offset of string
		
		INT 0x10			; call BIOS video service
		mov ah, 0x13		; service 13 - print string
		
		mov al, 1			; subservice 01 – update cursor 
 
		mov bh, 0			; output on page 0
		
		mov bl, 00110000B	; normal attrib
		mov cx, 33		; length of string
		mov dx, 0x0c00		; row 10 column 10
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, rule2		; bp = offset of string
		
		INT 0x10			; call BIOS video service

		mov ah, 0x13		; service 13 - print string
		
		mov al, 1			; subservice 01 – update cursor 
 
		mov bh, 0			; output on page 0
		
		mov bl, 00110000B	; normal attrib
		mov cx, 32		; length of string
		mov dx, 0x0e00		; row 10 column 10
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, rule3		; bp = offset of string
		
		INT 0x10			; call BIOS video service		
		
				mov ah, 0x13		; service 13 - print string
		
		mov al, 1			; subservice 01 – update cursor 
 
		mov bh, 0			; output on page 0
		
		mov bl, 10110000B	; normal attrib
		mov cx, 23		; length of string
		mov dx, 0x1300		; row 10 column 10
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, pressenter		; bp = offset of string
		
		INT 0x10			; call BIOS video service
		mov ah, 0x13		; service 13 - print string
		
		mov al, 1			; subservice 01 – update cursor 
 
		mov bh, 0			; output on page 0
		
		mov bl, 00110000B	; normal attrib
		mov cx, 79		; length of string
		mov dx, 0x1700		; row 10 column 10
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, developer		; bp = offset of string
		
		INT 0x10			; call BIOS video service
		
			mov dx, startgame	 						; ds:dx points to '$' terminated string
			mov ah, 9 								; service 9 –  WRITE STRING TO STANDARD OUTPUT
			int 0x21 								; dos services
			
			mov cx, [maxlength] ; load maximum length in cx
			mov si, buffer ; point si to start of buffer
			nextchar1: mov ah, 1 ; service 1 – read character
			int 0x21 ; dos services
			cmp al, 13 ; is enter pressed
			je exit3 ; yes, leave input
			mov [si], al ; no, save this character
			inc si ; increment buffer pointer
			loop nextchar1 ; repeat for next input char
			exit3: mov byte [si], '$' ; append $ to user input
			
			popa
			ret
			
sound:		push cx
			push ax


			mov cx, 5
looop1:      mov al, 0b6h
			out 43h, al

			;load the counter 2 value for d3
			mov ax, 1fb4h
			out 42h, al
			mov al, ah
			out 42h, al

			;turn the speaker on
			in al, 61h
			mov ah,al
			or al, 3h
			out 61h, al
			;call delay
			mov al, ah
			out 61h, al

			call delay

			;load the counter 2 value for a3
			mov ax, 152fh
			out 42h, al
			mov al, ah
			out 42h, al

			;turn the speaker on
			in al, 61h
			mov ah,al
			or al, 3h
			out 61h, al
			call delay
			mov al, ah
			out 61h, al

			call delay
	
			;load the counter 2 value for a4
			mov ax, 0A97h
			out 42h, al
			mov al, ah
			out 42h, al
	
			;turn the speaker on
			in al, 61h
			mov ah,al
			or al, 3h
			out 61h, al
			;call delay
			mov al, ah
			out 61h, al

			call delay
 
			loop looop1
			
			pop ax
			pop cx
			ret
			
printscore:	push bp
			mov bp, sp
			push es
			push di
			push ax
			push bx
			push cx
			push dx
			
							mov ah, 0x13		; service 13 - print string
		
		mov al, 1			; subservice 01 – update cursor 
 
		mov bh, 0			; output on page 0
		
		mov bl, 0x0b 	; normal attrib
		mov cx, 6		; length of string
		mov dx, 0x0044		; row 10 column 10
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, scorestr		; bp = offset of string
		
		INT 0x10

			mov ax, 0xb800
			mov es, ax ; point es to video base
			mov ax, [cs:score] ; load number in ax
			mov bx, 10 ; use base 10 for division
			mov cx, 0 ; initialize count of digits
			
nextindex: 	mov dx, 0 ; zero upper half of dividend
			div bx ; divide by 10
			add dl, 0x30 ; convert digit into ascii value
			push dx ; save ascii value on stack
			inc cx ; increment count of values
			cmp ax, 0 ; is the quotient zero
			jnz nextindex ; if no divide it again
			mov di, 150 ; point di to 70th column
			
nextprint: 	pop dx ; remove a digit from the stack
			mov dh, 0x0b ; use normal attribute    ;0011 1000
			mov [es:di], dx ; print char on screen
			add di, 2 ; move to next screen location
			loop nextprint ; repeat for all digits on stack
			
			
			pop dx
			pop cx
			pop bx
			pop ax 
			pop di
			pop es
			pop bp
			ret
			
exitinterface:	pusha

			call menuclrscr
			mov ah, 0x13		; service 13 - print string
		
		mov al, 1			; subservice 01 – update cursor 
 
		mov bh, 0			; output on page 0
		
		mov bl, 0x0b 	; normal attrib
		mov cx, 43		; length of string
		mov dx, 0x1215		; row 10 column 10
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, escapegame		; bp = offset of string
		
		INT 0x10
			
			popa
			ret

printingall:	pusha
				call clrscr
				call Sky
				call Buildings
				call Sea
				call Boats
				call fish
				call printscore
				popa
				ret

generaterandom:

push bp
mov bp,sp
pusha

   MOV AH, 00h    
   INT 1AH            

   mov  ax, dx
   xor  dx, dx
   mov  bx, 900   
   div  bx       
   
    mov bx,dx
    mov ax,dx
	mov dx,1
    mov cx,2
	div cx
	cmp dx,1
    jne even
   
odd:
   add bx, 2721
   jmp rand
   
even: 
   add bx, 2720

rand:  
	   mov [bp+4], bx
	   
popa
pop bp
ret
				
			
timer:					pusha
						push es
						inc word [cs: ticks]
						mov ax, [cs:ticks]
						call updatecoins
						cmp ax, 2
						jne  exitTimer
						call MoveLeft
						call MoveRight
						mov ax, 0
						mov [cs:ticks], ax
						
						mov ax,0
			mov ax, 0xb800
			mov es, ax						; point es to video memory			
		
            inc     word [cs: g_ticks]
						inc     word [cs: r_ticks]

            cmp     word [cs: g_ticks], 45      ; 18.2 ticks per second
            jb     redcoin

            greencoin:
			
			mov di,[cs:coinloc]
			mov word[es:di],0x3020
			
            push 0xFFFF
	        call generaterandom
			pop ax
	        mov di,ax
			mov word[cs:coinloc],ax
	        mov word[es:di],0x2020	        
			mov  word [cs: g_ticks], 0
			
            

            redcoin:
			cmp     word [cs: r_ticks], 20      ; 18.2 ticks per second
            jne     exitTimer
			mov di,[cs:coinloc+2]
			mov word[es:di],0x3020
            push 0x7FFF
	        call generaterandom
			pop ax
	        mov di,ax
			mov word[cs:coinloc+2],ax
	        mov word[es:di],0x4020	        
			mov  word [cs: r_ticks], 0
			
			
			

exitTimer:				mov     al, 0x20                                          ; send EOI
						out     0x20, al
						pop es
						popa
						jmp far [cs:oldtsr]                                                     ; return from interrupt
					

main:		   

            call interface	
			call printingall
			xor ax, ax
			mov es, ax								; point es to IVT base
			
			mov ax, [es:9*4]
			mov [oldisr], ax						; save offset of old routine
			mov ax, [es:9*4+2]
			mov [oldisr+2], ax	
			mov ax, [es:8*4]
			mov [oldtsr], ax						; save offset of old routine
			mov ax, [es:8*4+2]
			mov [oldtsr+2], ax						; save segment of old routine
			
			
			cli										; disable interrupts
			mov word [es:9*4], kbisr				; store offset at n*4
			mov [es:9*4+2], cs	
			mov word[es:8*4], timer; store segment at n*4+2
			mov [es:8*4+2], cs
			sti				; enable interrupts
			
			
			infLoop:
			
			cmp word[hookflag], 1
			jne infLoop
			
			mov ax , 0
			mov es, ax
			
			cli										; disable interrupts
			mov ax, [oldisr]				; store offset at n*4
			mov bx, [oldisr + 2]
            mov word [es:9*4], ax				; store offset at n*4
			mov [es:9*4+2], bx
			
			mov ax, [oldtsr]				; store offset at n*4
			mov bx, [oldtsr + 2]
            mov word [es:8*4], ax				; store offset at n*4
			mov [es:8*4+2], bx	
			sti	
			
			

			
			
			
ending: call clrscr


;Terminate Program
mov ax, 0x4c00
int 0x21