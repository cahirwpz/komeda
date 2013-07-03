TOPDIR = $(realpath $(CURDIR))

export TOPDIR

LDLIBS = -lmodules -Lmodules

OBJS = kmdplay.o synth.o interp.o player.o file.o libmodules.a

all:: .clang_complete kmdplay

kmdplay: $(OBJS)

clean::
	@make -C modules clean
	@rm -vf kmdplay
	@rm -vrf kmdplay.dSYM

include $(TOPDIR)/Makefile.common

# vim: sw=8 ts=8
