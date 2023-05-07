/* Copyright (C) 1984 by Manx Software Systems */

qsort(base, nel, size, compar)
char *base; unsigned nel, size; int (*compar)();
{
	register char *i,*j,*x,*r;
	auto struct stk {
		char *l, *r;
	} stack[16];
	struct stk *sp;

	sp = stack;
	r = base + (nel-1)*size;
	for (;;) {
		do {
			x = base + (r-base)/size/2 * size;
			i = base;
			j = r;
			do {
				while ((*compar)(i,x) < 0)
					i += size;
				while ((*compar)(x,j) < 0)
					j -= size;
				if (i < j) {
					swapmem(i, j, size);
					if (i == x)
						x = j;
					else if (j == x)
						x = i;
				}
				if (i <= j) {
					i += size;
					j -= size;
				}
			} while (i <= j);
			if (j-base < r-i) {
				if (i < r) {	/* stack request for right partition */
					sp->l = i;
					sp->r = r;
					++sp;
				}
				r = j;			/* continue sorting left partition */
			} else {
				if (base < j) {	/* stack request for left partition */
					sp->l = base;
					sp->r = j;
					++sp;
				}
				base = i;		/* continue sorting right partition */
			}
		} while (base < r);

		if (sp <= stack)
			break;
		--sp;
		base = sp->l;
		r = sp->r;
	}
}
