
all: build

test: build
	cabal v1-test

build: prepare
	cabal v1-configure -O0
	cabal v1-build

prepare: icfpc2024.cabal

icfpc2024.cabal: package.yaml
	hpack

clean:
	cabal v1-clean
