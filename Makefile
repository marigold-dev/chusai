VERSION := $(shell git describe)

MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
export BUILD_ROOT:= $(dir $(MAKEFILE_PATH))

build: layer1 layer2

layer1:
	make -C layer1 build

layer2:
	dune build


test-layer2:
    dune runtest --no-buffer -j 1

test-layer1:
	make -C layer1 test
	make -C integration_tests test

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
