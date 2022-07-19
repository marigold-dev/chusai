.PHONY: build layer1 layer2 test-layer1 test-layer2 test
.PHONY: clean package metrics opam-deps opam-dev-deps tezt get-tezos-binaries

VERSION := $(shell git describe)
BUILD_DIRECTORY:=_build/
MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
export BUILD_ROOT:= $(dir $(MAKEFILE_PATH))

# Defines the Tezos's binaries version and provider
TEZOS_BINARIES_VERSION:=v13.0-1
TEZOS_BINARIES_REPO:=https://github.com/serokell/tezos-packaging/releases/download/
TEZOS_BINARIES_URL:=$(TEZOS_BINARIES_REPO)$(TEZOS_BINARIES_VERSION)

build: layer1 layer2

layer1:
	make -C layer1 build

layer2:
	dune build


test-layer2:
	dune runtest --no-buffer -j 1

test-layer1:
	make -C layer1 test

tezt: build
	dune exec tezt/tezt_chusai.exe -- --verbose

test: test-layer1 test-layer2 tezt

clean:
	rm -rf _build

package: build
	tar czvf _build/chusai-layer1-$(VERSION).tar.gz _build/layer1/*.tez

metrics:
	make -C layer1 metrics

opam-deps:
	opam install . --deps-only --with-doc --with-test -y
	opam install tezt -y

opam-dev-deps:
	opam install utop merlin ocamlformat ocp-indent -y

# Retreives [tezos-node] and [tezos-client] (into [_build/])
get-tezos-binaries: make-build-dir
	wget -O $(BUILD_DIRECTORY)tezos-node $(TEZOS_BINARIES_URL)/tezos-node
	wget -O $(BUILD_DIRECTORY)tezos-client $(TEZOS_BINARIES_URL)/tezos-client
	chmod +x $(BUILD_DIRECTORY)tezos-node
	chmod +x $(BUILD_DIRECTORY)tezos-client

# Initialize the build directory
# (Even it should be done via `dune`)
make-build-dir:
	mkdir -p $(BUILD_DIRECTORY)
