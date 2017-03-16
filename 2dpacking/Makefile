.PHONY: build clean

build:
	# ghc_version 8.0.2
	cabal sandbox init
	cabal sandbox add-source ~/repos/github/minion-hs
	cabal install --dependencies-only
	cabal build

clean:
	cabal sandbox delete
	rm -rf dist
