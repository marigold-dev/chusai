VERSION := $(shell git describe)

.PHONY: all build clean dev-setup

_build:
	mkdir -p _build

build: _build
	make -C layer1 build
	echo FIXME: an unify way to build mint and wallet

test:
	make -C layer1 test
	make -C integration_tests test

clean:
	rm -rf _build

package: build
	tar czvf _build/chusai-layer1-$(VERSION).tar.gz _build/layer1/*.tez

metrics:
	make -C layer1 metrics


all: build

build:
	dune build

clean:
	dune clean

check-lint:
	dune build @fmt

lint:
	dune build @fmt --auto-promote

utop:
	dune utop

doc:
	dune build @doc

dev-setup:
	opam update
	opam switch create . ocaml-base-compiler.4.14.0 --deps-only -y
	eval $(opam env)
	opam install tezt -y

tezos-node:
	wget https://github.com/serokell/tezos-packaging/releases/download/v13.0-1/tezos-node
	chmod +x tezos-node

tezos-client:
	wget https://github.com/serokell/tezos-packaging/releases/download/v13.0-1/tezos-client
	chmod +x tezos-client

fetch-binaries: tezos-node tezos-client

tezt: build
	dune exec test/tezt/tezt_chusai.exe -- --verbose --regression-dir test/tezt/_regressions
