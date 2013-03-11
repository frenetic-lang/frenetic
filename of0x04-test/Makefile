all: of0x04-parsing make-messages

clean:
	rm -f of0x04-parsing make-messages of0x04-parsing.o make-messages.o cutils/open_memstream.o

CC=gcc
CFLAGS = -g -O0 -I . -I ../oflib-1.3/oflib/ -I ../oflib-1.3/lib -DHAVE_STRLCPY -DHAVE_FUNOPEN
LDFLAGS = ../oflib-1.3/lib/libopenflow.a

of0x04-parsing: of0x04-parsing.o cutils/open_memstream.o

make-messages: make-messages.o cutils/open_memstream.o
