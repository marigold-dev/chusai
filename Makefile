
.PHONY: build layer1 layer2 test-layer1 test-layer2 test clean package metrics opam-deps opam-dev-deps

VERSION := $(shell git describe)


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

test: test-layer1 test-layer2

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
