COMPILER_ARGS=--protocol ithaca
LIGO_VERSION=0.41.0
VERSION := $(shell git describe --always)
LIGO=docker run --rm -v $(PWD):$(PWD) -w $(PWD) ligolang/ligo:$(LIGO_VERSION)
export LIGO_BUILD=$(LIGO) compile contract $(COMPILER_ARGS)
export LIGO_TEST=$(LIGO) run test $(COMPILER_ARGS)
export LAYER1_BUILD_DIR=_build/layer1

.PHONY: build test metrics clean package

build: build-layer1 build-layer2
test: test-layer1 tezt
metrics: metrics-layer1

dev-setup:
	opam update
	opam switch create . ocaml-base-compiler.4.14.0 --deps-only -y
	eval $(opam env)
	opam install tezt -y

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

build-layer2:
	dune build


test-layer1:
	$(LIGO_TEST) layer1/stdlib_ext/test/test_main.mligo
	$(LIGO_TEST) layer1/wallet/test/test_main.mligo
	$(LIGO_TEST) layer1/mint/test/test_mint_sc.mligo

build-layer1:
	$(LIGO_BUILD) layer1/wallet/src/wallet_sc.mligo > $(LAYER1_BUILD_DIR)/wallet_sc.tez
	$(LIGO_BUILD) layer1/mint/src/mint_sc.mligo > $(LAYER1_BUILD_DIR)/mint_sc.tez

metrics-layer1:
	$(LIGO_TEST) layer1/wallet/metrics/metrics.mligo


package:
	tar czvf _build/chusai-layer1-$(VERSION).tar.gz _build/layer1/*.tez

tezos-node:
	wget https://github.com/serokell/tezos-packaging/releases/download/v13.0-1/tezos-node
	chmod +x tezos-node

tezos-client:
	wget https://github.com/serokell/tezos-packaging/releases/download/v13.0-1/tezos-client
	chmod +x tezos-client

fetch-binaries: tezos-node tezos-client

tezt: build
	dune exec integration_tests/tezt/tezt_chusai.exe -- --verbose --regression-dir integration_tests/tezt/_regressions
