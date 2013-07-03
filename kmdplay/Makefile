TOPDIR = $(realpath $(CURDIR))

export TOPDIR

OBJS = main.o synth.o interp.o player.o file.o

all:: .clang_complete kmdplay

kmdplay: $(OBJS)
	@echo "LINK $@"
	$(CC) -o $@ $^ $(LDLIBS)

clean::
	@rm -vf kmdplay
	@rm -vrf kmdplay.dSYM

include $(TOPDIR)/Makefile.common

# vim: sw=8 ts=8
