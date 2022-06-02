all: build

# Environment variables

# LIGO compiler version
LIGO_COMPILER_VERSION:=0.42.1

# Defines the targeted protocol (mainly for the production of Michelson files)
TEZOS_PROTOCOL:=ithaca

# Defines the Tezos's binaries version and provider
TEZOS_BINARIES_VERSION:=v13.0-1
TEZOS_BINARIES_REPO:=https://github.com/serokell/tezos-packaging/releases/download/
TEZOS_BINARIES_URL:=$(TEZOS_BINARIES_REPO)$(TEZOS_BINARIES_VERSION)

# Describes the compilation target. Even though the artefacts produced by Dunes
# are generated in this directory, it allows the Michelsons files to be located
# there as well.
BUILD_DIRECTORY:=_build/

# Where compiled contracts should be moved
COMPILED_CONTRACTS_DIRECTORY:=$(BUILD_DIRECTORY)contracts/

# $GIT_SHORT_HASH returns a short version of the commit hash. It is used to
# generate a tarball of the compiled Michelsons files.
GIT_SHORT_HASH := $(shell git describe --always)

# Run the Ligo compiler through a Docker image
LIGO_DOCKER := docker run --rm  -v $(PWD):$(PWD) -w $(PWD) ligolang/ligo:$(LIGO_COMPILER_VERSION)

# Functions

# Transforms [x] into [_build/x.tez].
define make_target
  $(strip $(COMPILED_CONTRACTS_DIRECTORY))$(strip $(1)).tez
endef

# Compiles the file [mligo] given as the 1st argument to the target given as the
# snd argument.
define build_contract
  $(LIGO_DOCKER) compile contract $(1) --protocol $(TEZOS_PROTOCOL) > $(call make_target, $(2))
endef

# Run a test file
define test_ligo
  $(LIGO_DOCKER) run test $(1) --protocol $(TEZOS_PROTOCOL)
endef

# Rules
.PHONY: test build michelson-tarball

build: build-layer1

build-layer1: build-layer1-bootstrap

# Build layer1/bootstrap
build-layer1-bootstrap: make-build-dir
	mkdir -p $(COMPILED_CONTRACTS_DIRECTORY)bootstrap
	$(call build_contract,layer1/bootstrap/wallet_sc.mligo,bootstrap/wallet_sc)
	$(call build_contract,layer1/bootstrap/mint_sc.mligo,bootstrap/mint_sc)

# Run all test-suites
test: test-layer1

# Run Layer-1 test-suites
test-layer1:
	$(call test_ligo,layer1/test/test.mligo)

michelson-tarball:
	tar czvf $(BUILD_DIRECTORY)contracts-$(GIT_SHORT_HASH).tar.gz \
	   -C $(COMPILED_CONTRACTS_DIRECTORY) .

# Chore rules
# A set of rules that are related to the maintenance of the project,
# chores. (Recovering artefacts, packages from OPAM etc.)
.PHONY: opam-deps opam-dev-deps get-tezos-binaries make-build-dir

# Get the pinned OPAM deps (and pinned ones)
opam-deps:
	opam install . --deps-only --with-doc --with-test -y
	opam install tezt -y

# Get the developping environment
opam-dev-deps:
	opam install utop merlin ocamlformat ocp-indent -y

# Retreives [tezos-node] and [tezos-client] (into [_build/])
get-tezos-binaries: make-build-dir
	wget -O $(BUILD_DIRECTORY)tezos-node $(TEZOS_BINARIES_URL)/tezos-node
	wget -O $(BUILD_DIRECTORY)tezos-client $(TEZOS_BINARIES_URL)/tezos-client
	chmod +x $(BUILD_DIRECTORY)tezos-node
	chmod +x $(BUILD_DIRECTORY)tezos-client

# Initialize the build directory
# (Even it should be done via `dune`)
make-build-dir:
	mkdir -p $(BUILD_DIRECTORY)
	mkdir -p $(COMPILED_CONTRACTS_DIRECTORY)
