.PHONY: all configure build tags clean
all: configure build

configure:
	cabal configure

build:
	cabal build

clean:
	cabal clean

tags:
	hasktags -c .

