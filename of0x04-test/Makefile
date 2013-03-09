all: of0x04-parsing

CC=gcc
CFLAGS = -I . -I ../oflib-1.3/oflib/ -DHAVE_STRLCPY -D_GNU_SOURCE
LDFLAGS = ../oflib-1.3/lib/libopenflow.a

of0x04-parsing: of0x04-parsing.c cutils/open_memstream.c
