COMPILER_ARGS=--protocol ithaca
LIGO_VERSION=0.41.0

LIGO=docker run --rm -v $(PWD)/layer1:$(PWD)/layer1 -w $(PWD)/layer1 ligolang/ligo:$(LIGO_VERSION)
export LIGO_BUILD=$(LIGO) compile contract $(COMPILER_ARGS)
export LIGO_TEST=$(LIGO)  run test $(COMPILER_ARGS)

build:
	make -C wallet build

test:
	make -C wallet test

clean:
	make -C wallet clean

metrics:
	make -C wallet metrics