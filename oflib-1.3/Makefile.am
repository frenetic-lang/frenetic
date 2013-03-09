# The goal of -Wno-syntax here is just to suppress the Automake warning
# about overriding distdir, below.
AUTOMAKE_OPTIONS = foreign -Wno-syntax subdir-objects
ACLOCAL_AMFLAGS = -I m4
SUBDIRS =

AM_CPPFLAGS = -g -O0 -DHAVE_STRLCPY -D_GNU_SOURCE
AM_CPPFLAGS += -I $(top_srcdir)/include
AM_CPPFLAGS += -I $(top_srcdir)/lib

AM_CFLAGS = -Wstrict-prototypes

AM_LDFLAGS = -export-dynamic

CLEANFILES =
DISTCLEANFILES =
EXTRA_DIST =
TESTS =
TESTS_ENVIRONMENT =
bin_PROGRAMS =
bin_SCRIPTS =
#dist_commands_DATA =
dist_man_MANS =
dist_pkgdata_SCRIPTS =
dist_sbin_SCRIPTS =
man_MANS =
noinst_HEADERS =
noinst_LIBRARIES =
noinst_PROGRAMS =
noinst_SCRIPTS =

EXTRA_DIST += README.hwtables soexpand.pl regress

ro_c = echo '/* -*- mode: c; buffer-read-only: t -*- */'

SUFFIXES = .in
.in:
	$(PERL) $(srcdir)/soexpand.pl -I$(srcdir) < $< | \
	    sed -e 's,[@]LOGDIR[@],$(LOGDIR),g' \
		-e 's,[@]PKIDIR[@],$(PKIDIR),g' \
                -e 's,[@]RUNDIR[@],$(RUNDIR),g' \
                -e 's,[@]pkgdatadir[@],$(pkgdatadir),g' \
                -e 's,[@]PERL[@],$(PERL),g' > $@

include lib/automake.mk
include oflib/automake.mk
include include/automake.mk
