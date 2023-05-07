/* Copyright (C) 1981,1982 by Manx Software Systems */
#include "stdio.h"

setbuf(stream, buffer)
register FILE *stream; char *buffer;
{
	if (stream->_buff)
		return;
	if (buffer) {
		stream->_buff = buffer;
		stream->_buflen = BUFSIZ;
	} else {
		stream->_buff = &stream->_bytbuf;
		stream->_buflen = 1;
	}
}

