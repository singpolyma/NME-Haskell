CFLAGS=-std=c99 -pedantic -O2
GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XNoCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.2

.PHONY: all shell clean doc install

all: example dist/build/libHSnme-$(VERSION).a dist/openpgp-$(VERSION).tar.gz

install: dist/build/libHSnme-$(VERSION).a
	cabal install

example: example.hs Text/NME.hs ext/NME.o
	ghc --make $(GHCFLAGS) -o $@ $^

shell:
	ghci $(GHCFLAGS)

report.html: Text/NME.hs
	-hlint $(HLINTFLAGS) --report $^

doc: dist/doc/html/nme/index.html README

README: nme.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\n,s/\\\\\\//\\//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/nme/index.html: dist/setup-config Text/NME.hs
	cabal haddock --hyperlink-source

dist/setup-config: nme.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist Test/NME.hs example

dist/build/libHSnme-$(VERSION).a: Text/NME.hs ext/NME.c dist/setup-config
	cabal build --ghc-options="$(GHCFLAGS)" --gcc-options="$(CFLAGS)"

dist/openpgp-$(VERSION).tar.gz: Text/NME.hs ext/NME.c ext/NME.h README dist/setup-config
	cabal check
	cabal sdist

.SUFFIXES: .hsc .hs

.hsc.hs:
	hsc2hs -o $@ $^
