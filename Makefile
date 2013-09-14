PREFIX ?= /usr/local
CABAL ?= cabal
VERSION ?= 0.5
PKG=wishsys_$(VERSION)
ORIGPKG=wishsys_$(VERSION).orig

all:
	$(CABAL) sandbox init
	$(CABAL) --enable-tests install

install:
	cp .cabal-sandbox/bin/wishsys $(PREFIX)/bin/wishsys

clean:
	rm -rf .cabal-sandbox

dist:
	tar --exclude='.git' -cf $(PKG).tar .
	gzip $(PKG).tar
	mv $(PKG).tar.gz $(ORIGPKG).tar.gz
