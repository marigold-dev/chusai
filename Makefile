VERSION := $(shell git describe)

MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
export BUILD_ROOT:= $(dir $(MAKEFILE_PATH))

build:
	make -C layer1 build

test:
	make -C layer1 test
	make -C integration_tests test

clean:
	rm -rf _build

package: build
	tar czvf _build/chusai-layer1-$(VERSION).tar.gz _build/layer1/*.tez

metrics:
	make -C layer1 metrics