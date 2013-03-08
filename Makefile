CC	= gcc -g
CFLAGS	= -O2 -Wall $(shell pkg-config --cflags portaudio-2.0 samplerate)
LDLIBS	= $(shell pkg-config --libs portaudio-2.0 samplerate)

all:	yamac yamaplay

yamac:	yamac.hs
	@ghc -o $@ $^

yamaplay: yamaplay.c

clean:
	@rm -vf *.hi *.o *~
	@rm -vf yamac
