COMPILER_ARGS=--protocol ithaca
LIGO_VERSION=0.41.0

CHUSAI_BUILD_DOCKER_IMAGE=chusai-build-$(VERSION)
LIGO_IMAGE=ligolang/ligo:$(LIGO_VERSION)

CURRENT_DIR=$(BUILD_ROOT)/layer1

DOCKER_RUN=docker run --rm -v $(BUILD_ROOT):$(BUILD_ROOT) -w $(CURRENT_DIR) -i

LIGO=$(DOCKER_RUN) $(LIGO_IMAGE)

export LIGO_BUILD=$(LIGO) compile contract $(COMPILER_ARGS)
export LIGO_TEST=$(LIGO) run test $(COMPILER_ARGS)

export IPFS_CID=$(DOCKER_RUN) $(CHUSAI_BUILD_DOCKER_IMAGE) /usr/bin/ipfs add --only-hash
export SED=$(DOCKER_RUN) $(CHUSAI_BUILD_DOCKER_IMAGE) /bin/sed
export EXPAND_VERSION=$(SED) -e 's/@@VERSION@@/$(VERSION)/g' 
export CUT=$(DOCKER_RUN) $(CHUSAI_BUILD_DOCKER_IMAGE) /usr/bin/cut
export CAT=$(DOCKER_RUN) $(CHUSAI_BUILD_DOCKER_IMAGE) /bin/cat
export GREP=$(DOCKER_RUN) $(CHUSAI_BUILD_DOCKER_IMAGE) /bin/grep

build: ipfs_image
	make -C wallet build
	make -C mint build

test:
	make -C wallet test
	make -C mint test

metrics:
	make -C wallet metrics

package: build
	make -C wallet package

ipfs_image:
		docker build --tag $(CHUSAI_BUILD_DOCKER_IMAGE) tools/ipfs

.PHONY: ipfs_image