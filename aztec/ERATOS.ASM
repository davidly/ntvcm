	extrn	.begin,.chl,.swt
	extrn	csave,cret,.move
	global	flags_,8191
	PUBLIC main_
main_:	lxi d,.2
	call csave
	LXI H,.1+0
	PUSH H
	CALL printf_
	POP D
	LXI H,1
	XCHG
	LXI H,-6-.2
	DAD SP
	MOV M,E
	INX H
	MOV M,D
	JMP .4
.3:
	LXI H,-6-.2
	DAD SP
	PUSH H
	MOV A,M
	INX H
	MOV H,M
	MOV L,A
	INX H
	XCHG
	POP H
	MOV M,E
	INX H
	MOV M,D
.4:
	LXI H,-6-.2
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	LXI H,10
	CALL .le
	JZ .5
	LXI H,0
	XCHG
	LXI H,-4-.2
	DAD SP
	MOV M,E
	INX H
	MOV M,D
	LXI H,0
	XCHG
	LXI H,2-.2
	DAD SP
	MOV M,E
	INX H
	MOV M,D
	JMP .7
.6:
	LXI H,2-.2
	DAD SP
	PUSH H
	MOV A,M
	INX H
	MOV H,M
	MOV L,A
	INX H
	XCHG
	POP H
	MOV M,E
	INX H
	MOV M,D
.7:
	LXI H,2-.2
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	LXI H,8190
	CALL .le
	JZ .8
	LXI H,1
	PUSH H
	LXI H,4-.2
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	LXI H,flags_
	DAD D
	POP D
	MOV M,E
	JMP .6
.8:
	LXI H,0
	XCHG
	LXI H,2-.2
	DAD SP
	MOV M,E
	INX H
	MOV M,D
	JMP .10
.9:
	LXI H,2-.2
	DAD SP
	PUSH H
	MOV A,M
	INX H
	MOV H,M
	MOV L,A
	INX H
	XCHG
	POP H
	MOV M,E
	INX H
	MOV M,D
.10:
	LXI H,2-.2
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	LXI H,8190
	CALL .le
	JZ .11
	LXI H,2-.2
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	LXI H,flags_
	DAD D
	mov a,m
	ora a
	JZ .12
	LXI H,2-.2
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	LXI H,2-.2
	DAD SP
	MOV A,M
	INX H
	MOV H,M
	MOV L,A
	DAD D
	INX H
	INX H
	INX H
	XCHG
	LXI H,0-.2
	DAD SP
	MOV M,E
	INX H
	MOV M,D
	LXI H,2-.2
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	LXI H,0-.2
	DAD SP
	MOV A,M
	INX H
	MOV H,M
	MOV L,A
	DAD D
	XCHG
	LXI H,-2-.2
	DAD SP
	MOV M,E
	INX H
	MOV M,D
.13:
	LXI H,-2-.2
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	LXI H,8190
	CALL .le
	JZ .14
	LXI H,0
	PUSH H
	LXI H,0-.2
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	LXI H,flags_
	DAD D
	POP D
	MOV M,E
	LXI H,0-.2
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	LXI H,-2-.2
	DAD SP
	PUSH H
	MOV A,M
	INX H
	MOV H,M
	MOV L,A
	DAD D
	XCHG
	POP H
	MOV M,E
	INX H
	MOV M,D
	JMP .13
.14:
	LXI H,-4-.2
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	XCHG
	INX H
	XCHG
	LXI H,-4-.2
	DAD SP
	MOV M,E
	INX H
	MOV M,D
.12:
	JMP .9
.11:
	JMP .3
.5:
	LXI H,-4-.2
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	PUSH D
	LXI H,.1+16
	PUSH H
	CALL printf_
	POP D
	POP D
	RET
.2 EQU -10
.1:
	DB 49,48,32,105,116,101,114,97,116,105,111,110,115,32,58
	DB 0,32,37,100,32,112,114,105,109,101,115,0
	extrn	printf_
	extrn	.le
	END
