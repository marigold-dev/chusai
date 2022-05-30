COMPILER_ARGS=--protocol ithaca
LIGO_VERSION=0.41.0
VERSION := $(shell git describe --always)
LIGO=docker run --rm -v $(PWD):$(PWD) -w $(PWD) ligolang/ligo:$(LIGO_VERSION)
export LIGO_BUILD=$(LIGO) compile contract $(COMPILER_ARGS)
export LIGO_TEST=$(LIGO) run test $(COMPILER_ARGS)
export LAYER1_BUILD_DIR=_build/layer1

.PHONY: build test metrics clean package

build: build-layer1
test: test-layer1
metrics: metrics-layer1

test-layer1:
	$(LIGO_TEST) layer1/stdlib_ext/test/test_main.mligo
	$(LIGO_TEST) layer1/wallet/test/test_main.mligo
	$(LIGO_TEST) layer1/mint/test/test_mint_sc.mligo

build-layer1:
	$(LIGO_BUILD) layer1/wallet/src/wallet_sc.mligo > $(LAYER1_BUILD_DIR)/wallet_sc.tez
	$(LIGO_BUILD) layer1/mint/src/mint_sc.mligo > $(LAYER1_BUILD_DIR)/mint_sc.tez

metrics-layer1:
	$(LIGO_TEST) layer1/wallet/metrics/metrics.mligo

clean:
	dune clean

package:
	tar czvf _build/chusai-layer1-$(VERSION).tar.gz _build/layer1/*.tez
