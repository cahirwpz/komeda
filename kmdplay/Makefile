CC	 = clang -g
CPPFLAGS = $(shell pkg-config --cflags portaudio-2.0)
CFLAGS	 = -O2 -Wall
LDLIBS	 = $(shell pkg-config --libs portaudio-2.0)

OBJS = main.o synth.o interp.o player.o

all: .clang_complete kmdplay

.clang_complete:
	echo $(CFLAGS) > $@

kmdplay: main.o synth.o player.o interp.o
	$(CC) -o $@ $^ $(LDLIBS)

NODEPS := clean
DEPFILES := $(patsubst %.o,.deps/%.P,$(OBJS))

ifeq ($(words $(findstring $(MAKECMDGOALS), $(NODEPS))), 0)
  -include $(DEPFILES)
endif

.deps/%.P: %.c
	@mkdir -p .deps
	$(CC) $(CPPFLAGS) -MM -o $@ $<

%.o: %.c .deps/%.P 
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

clean:
	@rm -vf *.o *~
	@rm -vf kmdplay
	@rm -vrf kmdplay.dSYM .deps
	@rm -vf .clang_complete

.PHONY: all clean
