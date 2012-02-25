CFLAGS=-std=c99 -pedantic -O2
GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XNoCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.1

.PHONY: all shell clean

nme: Main.hs Text/NME.hs ext/NME.o
	ghc --make $(GHCFLAGS) -o $@ $^

shell:
	ghci $(GHCFLAGS)

report.html: Text/NME.hs
	-hlint $(HLINTFLAGS) --report $^

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) Test/NME.hs nme

.SUFFIXES: .hsc .hs

.hsc.hs:
	hsc2hs -o $@ $^
