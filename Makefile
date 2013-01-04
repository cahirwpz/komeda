CC	= gcc
CFLAGS	= -O2 -Wall $(shell sdl-config --cflags)
LDLIBS	= $(shell sdl-config --libs)

all:	yamac yamaplay

yamac:	yamac.hs
	@ghc -o $@ $^

yamaplay: yamaplay.c

clean:
	@rm -vf *.hi *.o *~
	@rm -vf yamac
