PREFIX ?= /usr/local
CABAL ?= /usr/bin/cabal
VERSION ?= 0.5
TARBALL_NAME=wishsys_$(VERSION).orig.tar.gz
BRANCH=stable
FORMAT=tar.gz

all:
	$(CABAL) --enable-tests install

install:
	install -p -m 0555 dist/bin/wishsys $(PREFIX)/bin/
	install -p -m 0644 static/img/glyphicons-halflings-white.png $(PREFIX)/share/wishsys/static/img/
	install -p -m 0644 static/img/glyphicons-halflings.png $(PREFIX)/share/wishsys/static/img/
	install -p -m 0644 static/combined/j49BOPB6.css $(PREFIX)/share/wishsys/static/combined/
	install -p -m 0644 static/combined/DtHk9AMk.css $(PREFIX)/share/wishsys/static/combined/
	install -p -m 0644 static/css/normalize.css $(PREFIX)/share/wishsys/static/css/
	install -p -m 0644 static/css/bootstrap.css $(PREFIX)/share/wishsys/static/css/
	install -p -m 0644 static/css/legacy.css $(PREFIX)/share/wishsys/static/css/
	install -p -m 0644 config/sqlite.yml $(PREFIX)/share/wishsys/config/
	install -p -m 0644 config/keter.yaml $(PREFIX)/share/wishsys/config/
	install -p -m 0644 config/robots.txt $(PREFIX)/share/wishsys/config/
	install -p -m 0644 config/favicon.ico $(PREFIX)/share/wishsys/config/
	install -p -m 0644 config/settings.yml $(PREFIX)/share/wishsys/config/

clean:
	rm -rf config/client_session_key.aes
	rm -rf dist/
	rm -rf static/combined/j49BOPB6.css
	rm -rf wishsys_test.sqlite3

tarball:
	git archive $(BRANCH) --format=$(FORMAT) -o $(TARBALL_NAME)
