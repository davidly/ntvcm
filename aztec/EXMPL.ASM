	extrn	.begin,.chl,.swt
	extrn	csave,cret,.move
	PUBLIC main_
main_:	lxi d,.2
	call csave
	LXI H,.1+0
	PUSH H
	CALL printf_
	POP D
	LXI H,-76-.2
	DAD SP
	PUSH H
	CALL gets_
	POP D
	LXI H,-76-.2
	DAD SP
	PUSH H
	LXI H,.1+25
	PUSH H
	CALL printf_
	POP D
	POP D
	RET
.2 EQU -80
.1:
	DB 112,108,101,97,115,101,32,101,110,116,101,114,32,121,111
	DB 117,114,32,110,97,109,101,58,32,0,104,101,108,108,111
	DB 44,32,37,115,44,32,119,101,108,99,111,109,101,32,116
	DB 111,32,116,104,101,32,103,114,111,119,105,110,103,32,99
	DB 111,109,109,117,110,105,116,121,32,111,102,32,65,122,116
	DB 101,99,32,67,32,117,115,101,114,115,10,0
	extrn	gets_
	extrn	printf_
	END
