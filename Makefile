VERSION := $(shell git describe)

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