BUILD_DIR=../../_build/layer1/

build: src/wallet_sc.mligo
		mkdir -p $(BUILD_DIR)
		$(LIGO_BUILD) wallet/src/wallet_sc.mligo > $(BUILD_DIR)/wallet_sc.tez

test:
		$(LIGO_TEST) stdlib_ext/test/test_main.mligo
		$(LIGO_TEST) wallet/test/test_main.mligo

metrics:
		$(LIGO_TEST) wallet/metrics/metrics.mligo

clean:
		rm -rf $(BUILD_DIR)

.PHONY: test build clean metrics