TOPDIR = $(realpath $(CURDIR)/..)

export TOPDIR

OBJS = registry.o

all:: libmodules.a .clang_complete

libmodules.a: $(OBJS)

include $(TOPDIR)/Makefile.common

# vim: sw=8 ts=8
