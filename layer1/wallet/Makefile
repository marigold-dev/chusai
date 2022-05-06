COMPILER_ARGS=--protocol ithaca
LIGO=ligo compile contract $(COMPILER_ARGS) 
BUILD_DIR=../../_build/layer1/

TEST_MAIN=test/test_main.mligo

build: src/wallet_sc.mligo
		mkdir -p $(BUILD_DIR)
		$(LIGO) src/wallet_sc.mligo > $(BUILD_DIR)/wallet_sc.tez

test:
		ligo run test  $(COMPILER_ARGS) $(TEST_MAIN)

clean:
		rm -rf $(BUILD_DIR)

.PHONY: test build clean