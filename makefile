.PHONY: test

compile:
	cabal build

test:
	cabal test

clean:
	cabal clean

nix:
	nix build .#lspipe
