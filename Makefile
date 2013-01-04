yamac:	yamac.hs
	@ghc -o $@ $^

clean:
	@rm -vf *.hi *.o *~
	@rm -vf yamac
