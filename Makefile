export VERSION := $(shell git describe)
# https://stackoverflow.com/questions/18136918/how-to-get-current-relative-directory-of-your-makefile
MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
export BUILD_ROOT:= $(dir $(MAKEFILE_PATH))

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

package: 
	make -C layer1 package
	
metrics:
	make -C layer1 metrics