TOPDIR = $(realpath $(CURDIR))

export TOPDIR

LDLIBS = -lmodules -Lmodules

OBJS = kmdplay.o synth.o interp.o player.o file.o debug.o libmodules.a

all:: .clang_complete kmdplay

kmdplay: $(OBJS)

file.o: CFLAGS += -Wno-unused-function

clean::
	@make -C modules clean
	@rm -vf kmdplay
	@rm -vrf kmdplay.dSYM

lines:
	@find . -iname '*.c' -or -iname '*.h' | xargs sed '/^$$/d' | wc -l

include $(TOPDIR)/Makefile.common

# vim: sw=8 ts=8
