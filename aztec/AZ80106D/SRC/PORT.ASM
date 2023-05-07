;
;	Direct Port I/O Functions for AZTEC C II
;
;	Copyright (c) 1982 William C. Colley III
;
; I grant Manx Software Systems permission to incorporate these functions
; into the AZTEC C library subject only to the condition that my copyright
; notice remain in the source code.  WCC3.
;
; These functions allow AZTEC C II to get to the machine I/O ports.  They
; are more complicated than might be expected as they can't use the Z-80's
; "IN A,(C)" and "OUT (C),A" instructions and still remain 8080-compatible.
; Self-modifying code is also out of the question as that kills ROMability.
; I therefore go through the hassle of setting up temporary subroutines in
; RAM and calling them.
;
; The functions in the package are:
;
;	char in(p)			Returns contents of input port p.
;	char p;
;
;	out(p,c)			Sends character c to output port p.
;	char p, c;
;
		CSEG
		PUBLIC	in_, out_

;*****************************************************************************

in_:		LXI	H, 2		;Get port number from stack.
		DAD	SP
		MOV	H, M

		MVI	L, 0dbh		;Form input instruction of temporary
		SHLD	TMP		;  subroutine and set it up in core.

		LXI	H, TMP + 2	;Add return instruction to temporary
		MVI	M, 0c9h		;  subroutine in core.

		CALL	TMP		;Call temporary subroutine.

		MOV	L, A		;Return result.
		MVI	H, 0
		ORA	H
		RET

;*****************************************************************************

out_:		LXI	H, 4		;Get data and port number from stack.
		DAD	SP
		MOV	A, M
		DCX	H
		DCX	H
		MOV	H, M

		MVI	L, 0d3h		;Form output instruction of temporary
		SHLD	TMP		;  subroutine and set it up in core.

		LXI	H, TMP + 2	;Add return instruction to temporary
		MVI	M, 0c9h		;  subroutine in core.

		JMP	TMP		;Call temporary subroutine and return.

;*****************************************************************************

		DSEG

TMP:		DS	3		;Space for temporary subroutine.

;*****************************************************************************

		END
