.PHONY: test

compile:
	cabal build

test:
	cabal test

clean:
	cabal clean

nix:
	nix build .#lspipe

watch:
	ghciwatch --command "cabal v2-repl lib:lspipe" --watch lib
