PREFIX ?= /usr/local
CABAL ?= cabal

all:
	$(CABAL) sandbox init
	$(CABAL) --enable-tests install

install:
	cp .cabal-sandbox/bin/wishsys $(PREFIX)/bin/wishsys

clean:
	rm -rf .cabal-sandbox
