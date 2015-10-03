
all: site view
	cd site; grunt watch

site: lib
	cabal exec ink-site
	cd site; grunt

lib:
	cabal install

view:
	google-chrome site/dist/index.html

.PHONY: all site lib view

