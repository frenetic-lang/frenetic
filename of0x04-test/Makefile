all: of0x04-parsing make-messages

clean:
	rm -f of0x04-parsing make-messages

CC=gcc
CFLAGS = -g -O0 -I . -I ../oflib-1.3/oflib/ -I ../oflib-1.3/lib -DHAVE_STRLCPY
LDFLAGS = ../oflib-1.3/lib/libopenflow.a

of0x04-parsing: of0x04-parsing.c cutils/open_memstream.c

make-messages: make-messages.c cutils/open_memstream.c
