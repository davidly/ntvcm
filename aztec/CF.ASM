	extrn	.begin,.chl,.swt
	extrn	zsave,zret
	global	g_acbuff_,128
	DSEG
	public	g_pbuf_
g_pbuf_:
	DW g_acbuff_
	CSEG
	PUBLIC strchr_
strchr_:	lxi d,.2
	call zsave
.3:
	LXI H,12-.2
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	XCHG
	mov a,m
	ora a
	JZ .4
	LXI H,14-.2
	DAD SP
	MOV E,M
	MVI	D,0
	PUSH D
	LXI H,14-.2
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	XCHG
	MOV E,M
	MVI	D,0
	POP H
	CALL .eq
	JZ .5
	LXI H,12-.2
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	XCHG
	RET
.5:
	LXI H,12-.2
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
	JMP .3
.4:
	LXI H,0
	RET
.2 EQU 0
	PUBLIC main_
main_:	lxi d,.6
	call zsave
	LXI H,-1
	XCHG
	LXI H,0-.6
	DAD SP
	MOV M,E
	INX H
	MOV M,D
	LXI H,-1
	XCHG
	LXI H,-2-.6
	DAD SP
	MOV M,E
	INX H
	MOV M,D
	LXI H,3
	XCHG
	LXI H,12-.6
	DAD SP
	MOV A,M
	INX H
	MOV H,M
	MOV L,A
	CALL .eq
	JZ .7
	LXI H,63
	PUSH H
	LXI H,16-.6
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	XCHG
	INX H
	INX H
	MOV E,M
	INX H
	MOV D,M
	PUSH D
	CALL strchr_
	POP D
	POP D
	JNZ .9
	LXI H,42
	PUSH H
	LXI H,16-.6
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	XCHG
	INX H
	INX H
	MOV E,M
	INX H
	MOV D,M
	PUSH D
	CALL strchr_
	POP D
	POP D
	JNZ .9
	LXI H,63
	PUSH H
	LXI H,16-.6
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	LXI H,4
	DAD D
	MOV E,M
	INX H
	MOV D,M
	PUSH D
	CALL strchr_
	POP D
	POP D
	JNZ .9
	LXI H,42
	PUSH H
	LXI H,16-.6
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	LXI H,4
	DAD D
	MOV E,M
	INX H
	MOV D,M
	PUSH D
	CALL strchr_
	POP D
	POP D
	JZ .8
.9:
	LXI H,.1+0
	PUSH H
	CALL puts_
	POP D
	JMP .10
.8:
	LXI H,0
	PUSH H
	LXI H,16-.6
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	XCHG
	INX H
	INX H
	MOV E,M
	INX H
	MOV D,M
	PUSH D
	CALL open_
	POP D
	POP D
	XCHG
	LXI H,0-.6
	DAD SP
	MOV M,E
	INX H
	MOV M,D
	LXI H,-1
	XCHG
	LXI H,0-.6
	DAD SP
	MOV A,M
	INX H
	MOV H,M
	MOV L,A
	CALL .eq
	JZ .11
	LXI H,.1+36
	PUSH H
	CALL puts_
	POP D
	JMP .12
.11:
	LXI H,0
	PUSH H
	LXI H,258
	PUSH H
	LXI H,18-.6
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	LXI H,4
	DAD D
	MOV E,M
	INX H
	MOV D,M
	PUSH D
	CALL open_
	POP D
	POP D
	POP D
	XCHG
	LXI H,-2-.6
	DAD SP
	MOV M,E
	INX H
	MOV M,D
	LXI H,-1
	XCHG
	LXI H,-2-.6
	DAD SP
	MOV A,M
	INX H
	MOV H,M
	MOV L,A
	CALL .eq
	JZ .13
	LXI H,.1+63
	PUSH H
	CALL puts_
	POP D
	JMP .14
.13:
.15:
	LXI H,1
	MOV A,H
	ORA L
	JZ .16
	LXI H,.1+95
	PUSH H
	CALL printf_
	POP D
	LXI H,128
	PUSH H
	LXI H,g_acbuff_
	PUSH H
	LXI H,4-.6
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	PUSH D
	CALL read_
	POP D
	POP D
	POP D
	XCHG
	LXI H,6-.6
	DAD SP
	MOV M,E
	INX H
	MOV M,D
	LXI H,6-.6
	DAD SP
	mov a,m
	inx h
	ora m
	JZ .16
	LXI H,6-.6
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	PUSH D
	LXI H,.1+107
	PUSH H
	CALL printf_
	POP D
	POP D
	LXI H,6-.6
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	PUSH D
	LXI H,g_acbuff_
	PUSH H
	LXI H,2-.6
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	PUSH D
	CALL write_
	POP D
	POP D
	POP D
	JMP .15
.16:
.14:
.12:
.10:
	LXI H,-1
	XCHG
	LXI H,0-.6
	DAD SP
	MOV A,M
	INX H
	MOV H,M
	MOV L,A
	CALL .ne
	JZ .17
	LXI H,0-.6
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	PUSH D
	CALL close_
	POP D
.17:
	LXI H,-1
	XCHG
	LXI H,-2-.6
	DAD SP
	MOV A,M
	INX H
	MOV H,M
	MOV L,A
	CALL .ne
	JZ .18
	LXI H,-2-.6
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	PUSH D
	CALL close_
	POP D
.18:
	JMP .19
.7:
	LXI H,.1+128
	PUSH H
	CALL puts_
	POP D
.19:
	RET
.6 EQU -10
.1:
	DB 119,105,108,100,99,97,114,100,32,99,111,112,105,101,115
	DB 32,97,114,101,32,110,111,116,32,105,109,112,108,101,109
	DB 101,110,116,101,100,0,117,110,97,98,108,101,32,116,111
	DB 32,111,112,101,110,32,115,111,117,114,99,101,32,102,105
	DB 108,101,0,117,110,97,98,108,101,32,116,111,32,111,112
	DB 101,110,32,100,101,115,116,105,110,97,116,105,111,110,32
	DB 102,105,108,101,0,114,101,97,100,105,110,103,46,46,46
	DB 10,0,119,114,105,116,105,110,103,46,46,46,32,37,100
	DB 32,98,121,116,101,115,10,0,99,111,112,121,32,101,120
	DB 112,101,99,116,115,32,50,32,97,114,103,117,109,101,110
	DB 116,115,0
	extrn	close_
	extrn	write_
	extrn	read_
	extrn	printf_
	extrn	open_
	extrn	puts_
	extrn	.eq
	extrn	.ne
	END
