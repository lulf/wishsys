### Commented entries have reasonable defaults.
### Uncomment to edit them.
# Source: <source package name; defaults to package name>
Section: misc
Priority: optional
# Homepage: <enter URL here; no default>
Standards-Version: 3.9.2

Package: wishsys
Version: 1.0
Maintainer: Ulf Lilleengen <ulf_lillengen@yahoo.com>
# Pre-Depends: <comma-separated list of packages>
# Depends: <comma-separated list of packages>
# Recommends: <comma-separated list of packages>
# Suggests: <comma-separated list of packages>
# Provides: <comma-separated list of packages>
# Replaces: <comma-separated list of packages>
# Architecture: all
# Copyright: <copyright file; defaults to GPL2>
# Changelog: <changelog file; defaults to a generic changelog>
Readme: README
Extra-Files: README
Files: debian/initscript /etc/init.d/wishsys
  cabal-dev/bin/wishsys /usr/bin/wishsys
  static/img/glyphicons-halflings-white.png /usr/share/wishsys
  static/img/glyphicons-halflings.png /usr/share/wishsys
  static/combined/j49BOPB6.css /usr/share/wishsys
  static/combined/DtHk9AMk.css /usr/share/wishsys
  static/css/normalize.css /usr/share/wishsys
  static/css/bootstrap.css /usr/share/wishsys
  static/css/legacy.css /usr/share/wishsys
  config/sqlite.yml /usr/share/wishsys
  config/keter.yaml /usr/share/wishsys
  config/client_session_key.aes /usr/share/wishsys
  config/robots.txt /usr/share/wishsys
  config/favicon.ico /usr/share/wishsys
  config/settings.yml /usr/share/wishsys

#  <more pairs, if there's more than one file to include. Notice the starting space>
Description: Wishsys web service
  `cat README`


