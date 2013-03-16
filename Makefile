CC	= clang -g
CFLAGS	= -O2 -Wall $(shell pkg-config --cflags portaudio-2.0 samplerate)
LDLIBS	= $(shell pkg-config --libs portaudio-2.0 samplerate)

all:	.clang_complete yamac yamaplay

yamac:	yamac.hs
	@ghc -o $@ $^

.clang_complete:
	echo $(CFLAGS) > $@

yamaplay: yamaplay.c

clean:
	@rm -vf *.hi *.o *~
	@rm -vf yamac yamaplay
	@rm -vrf yamaplay.dSYM
	@rm -vf .clang_complete
