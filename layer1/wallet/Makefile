BUILD_DIR=$(BUILD_ROOT)/_build
LAYER1_BUILD_DIR=$(BUILD_DIR)/layer1/
METADATA_FILENAME=wallet_sc-metadata-$(VERSION).json
OUTPUT_METADATA=$(LAYER1_BUILD_DIR)/$(METADATA_FILENAME)
OUTPUT_TEZ=$(LAYER1_BUILD_DIR)/wallet_sc-$(VERSION).tez

IPFS_METADATA_HASH=$(shell $(CAT) $(OUTPUT_METADATA) \
			| $(IPFS_CID) --stdin-name $(METADATA_FILENAME) 2>/dev/null \
			| $(CUT) -d ' ' -f2)
IPFS_METADATA_ADDRESS=/ipfs/$(IPFS_METADATA_HASH)

build: src/wallet_sc.mligo
		mkdir -p $(LAYER1_BUILD_DIR)
		$(LIGO_BUILD) wallet/src/wallet_sc.mligo > $(OUTPUT_TEZ)
		$(EXPAND_VERSION) wallet/metadata.json > $(OUTPUT_METADATA)
		echo Ipfs address: $(IPFS_METADATA_ADDRESS)

test:
		$(LIGO_TEST) stdlib_ext/test/test_main.mligo
		$(LIGO_TEST) wallet/test/test_main.mligo

metrics:
		$(LIGO_TEST) wallet/metrics/metrics.mligo

clean:
		rm -rf $(BUILD_DIR)

package:
		tar czvf $(BUILD_DIR)/chusai-layer1-$(VERSION).tar.gz $(LAYER1_BUILD_DIR)

.PHONY: test build clean metrics 
