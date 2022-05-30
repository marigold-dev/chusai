build: src/wallet_sc.mligo
		$(LIGO_BUILD) wallet/src/wallet_sc.mligo > $(LAYER1_BUILD_DIR)/wallet_sc.tez

test:
		$(LIGO_TEST) stdlib_ext/test/test_main.mligo

metrics:
		$(LIGO_TEST) wallet/metrics/metrics.mligo

.PHONY: test build metrics