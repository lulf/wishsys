PREFIX ?= /usr/local
CABAL ?= cabal
VERSION ?= 0.5

all:
	$(CABAL) sandbox init
	$(CABAL) --enable-tests install

install:
	cp .cabal-sandbox/bin/wishsys $(PREFIX)/bin/wishsys

clean:
	rm -rf .cabal-sandbox

dist:
	tar -cf wishsys_$(VERSION).tar .
	gzip wishsys_$(VERSION).tar
