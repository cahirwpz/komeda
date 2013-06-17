CC	= clang -g
CFLAGS	= -O2 -Wall $(shell pkg-config --cflags portaudio-2.0 samplerate)
LDLIBS	= $(shell pkg-config --libs portaudio-2.0 samplerate)

all:	.clang_complete kmdc kmdplay

kmdc:	kmdc.hs
	@ghc -o $@ $^

.clang_complete:
	echo $(CFLAGS) > $@

kmdplay: kmdplay.c

clean:
	@rm -vf *.hi *.o *~
	@rm -vf kmdc kmdplay
	@rm -vrf kmdplay.dSYM
	@rm -vf .clang_complete
