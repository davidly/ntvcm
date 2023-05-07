;Copyright (C) 1983 by Manx Software Systems
	public setmem_
setmem_: push b
	lxi h,4
	dad sp
	mov e,m
	inx h
	mov d,m
	inx h
	mov c,m
	inx h
	mov b,m
	inx h
	mov l,m
	xchg
setloop:
	mov a,b
	ora c
	jz done
	mov m,e
	inx h
	dcx b
	jmp setloop
done: pop b
	ret
	end
