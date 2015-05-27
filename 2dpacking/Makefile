.PHONY: build clean

build:
	cabal sandbox init
	cabal sandbox add-source ~/repos/stacs_cp/minion-hs
	cabal install --dependencies-only
	cabal build

clean:
	cabal sandbox delete
	rm -rf dist
