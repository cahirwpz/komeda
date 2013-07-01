CC	= clang -g
CFLAGS	= -O2 -Wall $(shell pkg-config --cflags portaudio-2.0 samplerate)
LDLIBS	= $(shell pkg-config --libs portaudio-2.0 samplerate)

all::	.clang_complete kmdplay

.clang_complete:
	echo $(CFLAGS) > $@

kmdplay: kmdplay.c

clean::
	@rm -vf *.o *~
	@rm -vf kmdplay
	@rm -vrf kmdplay.dSYM
	@rm -vf .clang_complete
