; Programa: Nr. 2
; Užduoties sąlyga: 11
; Atliko: Deividas Zaleskis

;Asemblerio išeities tekstų syntax highlight‘as. Skaito asemblerio išeities tekstą, rašo validų HTML failą, kuriame vaizduojamas tas asemblerinis failas nuspalvinant sintaksės elementus: žymes, instrukcijas, registrus, betarpiškus operandus ir eilučių literalus, komentarus.

.model small
.stack 100H

BUFFER_SIZE = 500
INSTRUCTIONS_SIZE = 512
REGISTERS_SIZE = 62+1
HTML_START_SIZE = 75+5
HTML_END_SIZE = 15+6
HTML_COLOR_START_SIZE = 13
HTML_COLOR_END_SIZE = 7
HTML_COLOR_CODE_SIZE = 9
HTML_BREAK_SIZE = 5



STATE_LABEL = 0
STATE_INSTR = 1
STATE_REG = 2
STATE_LIT = 3
STATE_IMM = 4
STATE_CMMT = 5
STATE_TXT = 6

.data

;vartotojo sasajai
help_msg db "programa keicia .asm source failus i html formato failus su sintakses paryskinimu kodui", 13, 10, "$"

;darbui su simboliais
buffer db BUFFER_SIZE dup(?)
last_char db ?
count dw 1
state_count dw 0
cx_count dw 0
already_written dw 0
state db 0
matching_state_reg db 0
    
;darbui su failais
src_file db 12 dup (0)
src_file_handle	dw ?
dest_file db 12 dup (0)
dest_file_handle dw ?

;sintakses ryskinimui
html_start db "<!DOCTYPE html> <html> <head> <title>Highlighted ASM</title> </head> <body>","<pre>"
html_end db "</pre>", "</body>", 10, "</html>"
html_color_start db "<font color=", 34
html_color_end db "</font>"
html_break db "<br>", 10
html_tab db ""
;color codes
html_color_code_literal db "#000080", 34, 62
html_color_code_register db "#6495ED", 34, 62
html_color_code_instruction db "#7FFF00", 34, 62
html_color_code_immediate db "#E9967A", 34, 62
html_color_code_comment db "#808080", 34, 62
html_color_code_label db "#700390", 34, 62
html_color_code_text db "#000000", 34, 62

;registrai ir operacijos palyginimams
registers db "AX.AH,AL,BX,BH,BL,CX,CH,CL,DX,DH,DL,CS,DS,ES,SS,IP,SP,BP,SI,DI."
;instructions db "AAA,AAD,AAM,AAS,ADC,ADD,AND,CALL,CBW,CLC,CLD,CLI,CMC,CMP,CMPSB,CMPSW,CWD,DAA,DAS,DEC,DIV,HLT,IDIV,IMUL,IN,INC,INT,INTO,IRET,JA,JAE,JB,JBE,JC,JCXZ,JE,JG,JGE,JL,JLE,JMP,JNA,JNAE,JNB,JNBE,JNC,JNE,JNG,JNGE,JNL,JNLE,JNO,JNP,JNS,JNZ,JO,JP,JPE,JPO,JS,JZ,LAHF,LDS,LEA,LES,LODSB,LODSW,LOOP,LOOPE,LOOPNE,LOOPNZ,LOOPZ,MOV,MOVSB,MOVSW,MUL,NEG,NOP,NOT,OR,OUT,POP,POPA,POPF,PUSH,PUSHA,PUSHF,RCL,RCR,REP,REPE,REPNE,REPNZ,REPZ,RET,RETF,ROL,ROR,SAHF,SAL,SAR,SBB,SCASB,SCASW,SHL,SHR,STC,STD,STI,STOSB,STOSW,SUB,TEST,XCHG,XLATB,XOR"

.code

start:
    mov	ax, @data
	mov	es, ax			; es kad galetume naudot stosb funkcija: Store AL at address ES:(E)DI
 
	mov	si, 81h        		; programos paleidimo parametrai rasomi segmente es pradedant 129 (arba 81h) baitu        
 
	call	skip_spaces
	
	mov	al, byte ptr ds:[si]	; nuskaityti pirma parametro simboli
	cmp	al, 13			; jei nera parametru
	je	help			; tai isvesti pagalba
	
	;; ar reikia isvesti pagalba
	mov	ax, word ptr ds:[si]
	cmp	ax, 3F2Fh        	; jei nuskaityta "/?" - 3F = '?'; 2F = '/'
	je	help                 	; rastas "/?", vadinasi reikia isvesti pagalba
	
	;; source failo pav. nuskaitymas
	lea	di, src_file
	call	read_filename		; perkelti is parametro i eilute
	cmp	byte ptr es:[src_file], '$' ; jei nieko nenuskaite
	je	help

	;; destination failo pavadinimo nusk.
	lea	di, dest_file
	call	read_filename		; perkelti is parametro i eilute

	push	ds si ; turbut kad pushina abu? paklaust destytojo
	

	mov	ax, @data
	mov	ds, ax
	
		;; skaitymui
	mov	dx, offset src_file	; ikelti i dx src failo pavadinima
	mov	ah, 3Dh			; ikelia komandos koda
	mov	al, 0   ; skaitymui
	int	21h			; INT 21h / AH= 3Dh - open existing file
	
	;TODO - cia turetu buti error handlinimas su carry flag'u, dabar nesvarbu
	mov	src_file_handle, ax		; issaugom handle, kadangi interruptas handle paduoda i ax
	
	;; rasymui
	mov	dx, offset dest_file	; ikelti i dx destination failo pavadinima
	mov	ah, 3Ch			; ikelia komandos koda
	mov	cx, 0			; normal - no attributes
	int	21h			; INT 21h / AH= 3Ch - create or truncate file.
	
	;TODO - cia turetu buti error handlinimas su carry flag'u, dabar nesvarbu
	mov	dest_file_handle, ax		; issaugom handle, kadangi interruptas handle paduoda i ax
	JMP prepare_html
	
help:
	mov	ax, @data
	mov	ds, ax
	
	mov	dx, offset help_msg         
	mov	ah, 09h
	int	21h
	JMP end_

prepare_html:
   
    MOV bx, dest_file_handle
    call write_html_start

main_loop:
    MOV bx, src_file_handle
    call read_file_to_buffer
    CMP [count], 0
    JE close_files
    
    MOV bx, dest_file_handle
    
    push cx ax
    call look_for_state
    pop ax cx
    ;call write_from_buffer
    JMP main_loop


close_files:
    ;uzdaryti abu failus pries baigiant darba
    MOV bx, dest_file_handle
    call write_html_end
    
    MOV ah, 3Eh
    MOV bx, src_file_handle
    INT 21h
    MOV bx, dest_file_handle
    INT 21h

end_:
    ;end program, return to DOS
    MOV ax, 4c00h 
	INT 21h


;subroutines

;TODO!

;STATE_INSTR = 1
;STATE_REG = 2


;###################

look_for_state PROC near

    MOV si, offset buffer
    MOV cx, [count]
    MOV [already_written], 0
    MOV [state_count], 0
    JMP state_loop
reset_state:
    
    MOV [state], STATE_TXT
    CMP cx, 0
    JNE state_loop
    JMP end_state_and_return
state_loop:
    LODSB
    INC [state_count]
check_for_new_state:
    CMP al, 10 ; look for newlines, if found end current state
    JE end_state_and_return
    
    CMP [state], STATE_CMMT ; comments should drown out everything else, so in STATE_CMMT the check is skipped
    JE skip_loop
    
    CMP al, 34 ; look for "", probably a literal
    JE check_for_literal
    
    CMP al, 39 ; look for '', probably a literal
    JE check_for_literal
    
    CMP al, 59 ; look for ; as this means a comment
    JE check_for_comment
    
    CMP [state], STATE_TXT
    JNE skip_loop

    CMP al, ':'
    JE check_for_label
    
    ;CMP al, 'A'
    ;JNB check_for_instruction1
    
    ;CMP al, 'a'
    ;JNB check_for_instruction1
    
    CMP al, 48 ; look for immediate or reg must be >= 0 ( ASCII 48)
    JNB check_for_immediate_step1
    
skip_loop:
    MOV [last_char], al
    LOOP state_loop
    ;if no state change is present, print and exit loop
    call end_current_state
    JMP end_state_loop

end_state_and_return:
    mov [last_char], al
    call end_current_state
    CMP cx, 0
    JNE reset_state
    JMP end_state_loop

;ALL CHECKS DONE HERE
check_for_comment:
    call end_current_state
    MOV [state], STATE_CMMT
    JMP state_loop
    
check_for_literal:
    call end_current_state
    CMP [state], STATE_LIT
    JNE set_state_literal
    JMP reset_state
set_state_literal:
    MOV [state], STATE_LIT
    JMP state_loop
check_for_label:
    MOV [state], STATE_LABEL
    JMP end_state_and_return

check_for_immediate_step1:
    CMP al, 57 ; <= 9 (ASCII 57)
    JNA check_for_immediate_step2
    JMP skip_loop
check_for_immediate_step2:
    CMP [last_char], 32 ; tarpas
    JE check_for_immediate_step3
    CMP [last_char], 9 ;tabas
    JE check_for_immediate_step3
    JMP skip_loop
check_for_immediate_step3:
    call end_current_state
    MOV [state], STATE_IMM
    call skip_to_space
    JMP end_state_and_return
check_for_instruction1:
    CMP al, 'Z'
    JNA check_for_instruction2
    JMP skip_loop
check_for_instruction2:
    DEC cx
    PUSH cx si
    call end_current_state
    MOV di, offset registers
    JMP check_match
check_other:
    pop si cx
    push cx si
    call skip_to_comma
    CMP byte ptr ds:[di], '.'
    JE end_match_loop
    INC di
    DEC si
    LODSB
    JMP check_match
no_match:
    CMP byte ptr ds:[di], '.'
    JE skip_this_check
    CMP byte ptr ds:[di], ','
    JNE check_other
skip_this_check:
    CMP al, ' '
    JE found_match
    CMP al, 10
    JE found_match
    JMP check_other
check_match:
    CMP byte ptr ds:[di], al
    JNE no_match
    LODSB
    INC di
    LOOP check_match
end_match_loop:
    pop si cx
    INC si
    DEC cx
    JMP reset_state
found_match:
    ADD sp, 4 ; si is at correct place already, so pop the stored value off the stack. also true for cx
    MOV ah, 02h
    MOV dl, 'a'
    INT 21h
    MOV [state], STATE_REG
    JMP end_state_and_return
    
end_state_loop:
    RET

look_for_state ENDP

;###################

skip_to_comma PROC near
loop_comma_skip:
    
    CMP byte ptr ds:[di], '.'
    JE end_comma_skip
    CMP byte ptr ds:[di], ','
    JE end_comma_skip
    INC di
    JMP loop_comma_skip
end_comma_skip:
    RET
skip_to_comma ENDP

skip_to_space PROC near
;skaitom kol baigiasi "zodis"
loop_skip:
    CMP cx, 0
    JE end_skip_loop
    
    LODSB
    INC [state_count]
    CMP al, 32 ; ieskome tarpo (ASCII 32)
    JE end_skip_loop
    CMP al, 44 ; arba kablelio (ASCII 44)
    JE end_skip_loop
    CMP al, 10 ; arba naujos eilutes (ASCII 10)
    JE end_skip_loop
    CMP al, 9  ; arba tabo (visi reiskia, jog zodis baigtas ir galima grizti)
    JE end_skip_loop
    
    MOV [last_char], al
    loop loop_skip
    
end_skip_loop:
    ret

skip_to_space ENDP

;###################

end_current_state PROC near

    DEC cx
    DEC [state_count];write to second last character (state change)
    push cx
    
    call write_from_buffer
    
    MOV cx, [state_count]
    ADD [already_written], cx
    pop cx
    MOV [state_count], 1 ;first character already counts
    ret

end_current_state ENDP
;###################

write_from_buffer PROC near
    
    CMP state, STATE_TXT
    JE write_buffer_content
    
    call write_html_color_start
    call write_html_color_code
    
write_buffer_content:
    ;push cx MAY BE CAUSE FOR NEW ERROR
    mov dx, offset buffer
    ADD dx, [already_written]
    mov cx, [state_count]
    MOV ah, 40h
    INT 21h
    ;pop cx THIS TOO
    
    CMP state, STATE_TXT
    JE write_from_buffer_end
    
    call write_html_color_end
    
write_from_buffer_end:
    RET
    
write_from_buffer ENDP

;###################
write_html_start PROC near
    mov dx, offset html_start
    mov cx, HTML_START_SIZE
    MOV ah, 40h
    INT 21h    
    ret
write_html_start ENDP

;###################

write_html_end PROC near
    mov dx, offset html_end
    mov cx, HTML_END_SIZE
    MOV ah, 40h
    INT 21h    
    ret
write_html_end ENDP

;###################

write_html_color_start PROC near
    mov dx, offset html_color_start
    mov cx, HTML_COLOR_START_SIZE
    MOV ah, 40h
    INT 21h    
    ret
write_html_color_start ENDP

;###################

write_html_color_end PROC near
    mov dx, offset html_color_end
    mov cx, HTML_COLOR_END_SIZE
    MOV ah, 40h
    INT 21h    
    ret
write_html_color_end ENDP

;###################


write_html_color_code PROC near

    mov cx, HTML_COLOR_CODE_SIZE
    MOV ah, 40h
    MOV dl, [state]
    
check_case:
    CMP dl, STATE_TXT
    JE write_text_code
    
    CMP dl, STATE_LIT
    JE write_literal_code
    
    CMP dl, STATE_REG
    JE write_register_code
    
    CMP dl, STATE_INSTR
    JE write_instruction_code
    
    CMP dl, STATE_IMM
    JE write_immediate_code
    
    CMP dl, STATE_CMMT
    JE write_comment_code
    
    CMP dl, STATE_LABEL
    JE write_label_code
    
write_text_code:
    mov dx, offset html_color_code_text
    INT 21h    
    ret
write_literal_code:
    mov dx, offset html_color_code_literal
    INT 21h    
    ret
write_register_code:
    mov dx, offset html_color_code_register
    INT 21h    
    ret

write_instruction_code:
    mov dx, offset html_color_code_instruction
    INT 21h    
    ret
write_immediate_code:
    mov dx, offset html_color_code_immediate
    INT 21h    
    ret
write_comment_code:
    mov dx, offset html_color_code_comment
    INT 21h    
    ret
write_label_code:
    mov dx, offset html_color_code_label
    INT 21h    
    ret
write_html_color_code ENDP

;###################

skip_spaces PROC near

skip_spaces_loop:
	cmp byte ptr ds:[si], ' '
	jne skip_spaces_end
	inc si
	jmp skip_spaces_loop
skip_spaces_end:
	ret
	
skip_spaces ENDP


;###################

read_filename PROC near

	push	ax
	call	skip_spaces
read_filename_start:
	cmp	byte ptr ds:[si], 13	; jei nera parametru
	je	read_filename_end	; tai baigtas failo vedimas
	cmp	byte ptr ds:[si], ' '	; tikriname ar yra tarpas
	jne	read_filename_next	; jei nera tarpo, tai skaito kita simboli, jei yra - skaitymas baigtas
read_filename_end:
	mov	al, '$'			; irasyti '$' gale
	stosb                           ; Store AL at address ES:(E)DI, di = di + 1
	pop	ax ; ax grizta i busena, buvusia pries subroutine call'a
	ret ;return to caller
read_filename_next:
	lodsb				; Load byte at DS:[SI] into AL. Update SI.
	stosb                           ; Store AL at address ES:(E)DI, update SI
	jmp read_filename_start

read_filename ENDP

;###################

read_file_to_buffer PROC near
    
    MOV bx, src_file_handle
    MOV dx, offset buffer
    MOV cx, BUFFER_SIZE
    
    MOV ah, 3Fh
    INT 21h
    MOV [count], ax    
    RET

read_file_to_buffer ENDP

;###################



end start