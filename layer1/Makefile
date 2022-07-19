COMPILER_ARGS=--protocol ithaca
LIGO_VERSION=0.43.0

LIGO=docker run --rm -v $(PWD):$(PWD) -w $(PWD) ligolang/ligo:$(LIGO_VERSION)

export LIGO_BUILD=$(LIGO) compile contract $(COMPILER_ARGS)
export LIGO_TEST=$(LIGO)  run test $(COMPILER_ARGS)

MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
export BUILD_ROOT:= $(dir $(MAKEFILE_PATH))/..

export LAYER1_BUILD_DIR=$(BUILD_ROOT)/_build/layer1

build: verifier
	mkdir -p $(LAYER1_BUILD_DIR)

	make -C wallet build
	make -C mint build
	make -C refutation build

verifier:
	make -C verifier build

test:
	$(LIGO_TEST) test/test.mligo

metrics:
	make -C wallet metrics


.PHONY: test build verifier metrics