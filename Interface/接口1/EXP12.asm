SSTACK SEGMENT STACK
    ;此处输入堆栈段代码
    DW 32 DUP(?)
SSTACK ENDS

CODE SEGMENT
    ASSUME CS:CODE
START:
	PUSH DS
    MOV AX,0000H
    MOV DS,AX
    ;此处输入代码段代码
    MOV AX,OFFSET MIR7
    MOV SI, 003CH
    MOV [SI],AX
    MOV AX,CS
    MOV SI,003EH
    MOV [SI],AX
    CLI
    POP DS
    
    MOV AL,11H
    OUT 20H,AL
    MOV AL,08H
    OUT 21H,AL
    MOV AL,04H
    OUT 21H,AL
    MOV AL,01H
    OUT 21H,AL
    MOV AL,6FH
    OUT 21H,AL
    STI
AA1:MOV AX, 0139H
	INT 10H
	CALL DELAY
	CALL DELAY
	CALL DELAY
	CALL DELAY
	CALL DELAY
	CALL DELAY
	CALL DELAY
	CALL DELAY
	CALL DELAY
	CALL DELAY
	CALL DELAY
	CALL DELAY
	JMP AA1
MIR7:STI
	CALL DELAY
	MOV AX, 0137H
	INT 10H
	MOV AX,0120H
	INT 10H
	MOV AL,20H
	OUT 20H,AL
	IRET
DELAY:PUSH CX
	MOV CX,0F00H
AA0:PUSH AX
	POP AX
	LOOP AA0
	POP CX
	RET
    MOV AH,4CH
    INT 21H
CODE ENDS
    END START
