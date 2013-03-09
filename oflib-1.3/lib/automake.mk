noinst_LIBRARIES += lib/libopenflow.a

lib_libopenflow_a_SOURCES = \
	lib/byte-order.h \
	lib/compiler.h \
	lib/dirs.c \
	lib/dirs.h \
	lib/dynamic-string.c \
	lib/dynamic-string.h \
	lib/fatal-signal.c \
	lib/fatal-signal.h \
	lib/flow.c \
	lib/flow.h \
	lib/hash.c \
	lib/hash.h \
	lib/hmap.c \
	lib/hmap.h \
	lib/ofp.c \
	lib/ofp.h \
	lib/ofpbuf.c \
	lib/ofpbuf.h \
	lib/packets.h \
	lib/random.c \
	lib/random.h \
	lib/sat-math.h \
	lib/timeval.c \
	lib/timeval.h \
	lib/type-props.h \
	lib/unaligned.h \
	lib/util.c \
	lib/util.h \
	lib/vlog-modules.def \
	lib/vlog.c \
	lib/vlog.h

lib_libopenflow_a_LIBADD = oflib/ofl-actions.o \
                           oflib/ofl-actions-pack.o \
                           oflib/ofl-actions-print.o \
                           oflib/ofl-actions-unpack.o \
                           oflib/ofl-messages.o \
                           oflib/ofl-messages-pack.o \
                           oflib/ofl-messages-print.o \
                           oflib/ofl-messages-unpack.o \
                           oflib/ofl-structs.o \
			               oflib/ofl-structs-match.o \
                           oflib/ofl-structs-pack.o \
                           oflib/ofl-structs-print.o \
                           oflib/ofl-structs-unpack.o \
                           oflib/oxm-match.o \
                           oflib/ofl-print.o

CLEANFILES += lib/dirs.c
lib/dirs.c: Makefile
		($(ro_c) && \
		 echo 'const char ofp_pkgdatadir[] = "$(pkgdatadir)";' && \
		 echo 'const char ofp_rundir[] = "@RUNDIR@";' && \
		 echo 'const char ofp_logdir[] = "@LOGDIR@";') > lib/dirs.c.tmp
		mv lib/dirs.c.tmp lib/dirs.c

