/* Copyright (C) 1981, 1982 by Manx Software Systems */

int errno;
#define FLT_FAULT	0		/* vector for floating-point faults */
#define	MAXVEC		4		/* reserve 4 vectors for now */
int (*Sysvec[MAXVEC])();

#define NULL 0
#define EOF -1
#define BUFSIZ 1024

#define _BUSY	0x01
#define _ALLBUF	0x02
#define _DIRTY	0x04
#define _EOF	0x08
#define _IOERR	0x10

typedef struct {
	char *_bp;			/* current position in buffer */
	char *_bend;		/* last character in buffer + 1 */
	char *_buff;		/* address of buffer */
	char _flags;		/* open mode, etc. */
	char _unit;			/* token returned by open */
	char _bytbuf;		/* single byte buffer for unbuffer streams */
	int	_buflen;		/* length of buffer */
} FILE;

#define MAXSTREAM 11
FILE Cbuffs[MAXSTREAM];
char *Stdbufs;			/* free list of buffers */
long ftell();

#define stdin (&Cbuffs[0])
#define stdout (&Cbuffs[1])
#define stderr (&Cbuffs[2])
#define getchar() agetc(stdin)
#define putchar(c) aputc(c, stdout)
#define feof(fp) (((fp)->_flags&_EOF)!=0)
#define ferror(fp) (((fp)->_flags&_IOERR)!=0)
#define clearerr(fp) ((fp)->_flags &= ~(_IOERR|_EOF))
#define fileno(fp) ((fp)->_unit)

#define O_RDONLY	0
#define O_WRONLY	1
#define O_RDWR		2
#define O_CREAT		0x0100
#define O_TRUNC		0x0200
#define O_EXCL		0x0400



/*
 * device control structure and blocks
 */
struct device {
	char d_read;
	char d_write;
	char d_ioctl;	/* used by character special devices (eg CON:) */
	char d_seek;	/* used by random I/O devices (eg: a file) */
	int (*d_open)();	/* for special open handling */
};
struct device condev,bdosout,bdosin,filedev;
/*
 * device table, contains names and pointers to device entries
 */
typedef char *arg;
struct devtabl {
	char *d_name;
	struct device *d_dev;
	arg d_arg;
};
#define DVTBLLEN 11 
struct devtabl devtabl[DVTBLLEN];

/*
 * channel table: relates fd's to devices
 */
struct channel {
	char c_read;
	char c_write;
	char c_ioctl;
	char c_seek;
	int (*c_close)();
	arg c_arg;
} ;
#define MAXCHAN	11	/* maximum number of I/O channels */
struct channel chantab[MAXCHAN];

