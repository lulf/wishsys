PREFIX ?= /usr/local
CABAL ?= $(HOME)/.cabal/bin/cabal
VERSION ?= 0.5
TARBALL_NAME=wishsys_$(VERSION).orig.tar.gz
BRANCH=stable
FORMAT=tar.gz

all:
	$(CABAL) sandbox init
	$(CABAL) --enable-tests install

install:
	install -p -m 0555 .cabal-sandbox/bin/wishsys $(PREFIX)/bin/wishsys

clean:
	rm -rf .cabal-sandbox

tarball:
	git archive $(BRANCH) --format=$(FORMAT) -o $(TARBALL_NAME)
