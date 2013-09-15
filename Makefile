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
	rm -rf cabal.sandbox.config
	rm -rf config/client_session_key.aes
	rm -rf dist/
	rm -rf static/combined/j49BOPB6.css
	rm -rf wishsys_test.sqlite3

tarball:
	git archive $(BRANCH) --format=$(FORMAT) -o $(TARBALL_NAME)
