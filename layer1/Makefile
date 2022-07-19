COMPILER_ARGS=--protocol jakarta
LIGO_VERSION=0.43.0

LIGO=docker run --rm -v $(PWD)/layer1:$(PWD)/layer1 -w $(PWD)/layer1 ligolang/ligo:$(LIGO_VERSION)

export LIGO_BUILD=$(LIGO) compile contract $(COMPILER_ARGS)
export LIGO_TEST=$(LIGO)  run test $(COMPILER_ARGS)
export LAYER1_BUILD_DIR=$(BUILD_ROOT)/_build/layer1

build:
	mkdir -p $(LAYER1_BUILD_DIR)

	make -C wallet build
	make -C mint build 
	make -C refutation build
	make -C chain build
	make -C bridge build

test:
	$(LIGO_TEST) test/test.mligo

metrics:
	make -C wallet metrics


.PHONY: test
