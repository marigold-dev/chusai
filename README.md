# Chusai

‍**Optimistic rollups** are a kind of Layer 2 that validate transactions on behalf of
the main chain. They are called optimistic, because they work off the assumption
that validation is correct until explicitly proven otherwise.
Even though Optimistic rollups don’t lead to faster finality, they increase the
throughput (_TPS_) significantly.

**Chusai** is an implementation of optimistic rollups that can run smart-contracts,
into smart-contracts, on top of [Tezos](https://tezos.com/).

## Setting up dev-env

> **Warning** 
> [Docker](https://www.docker.com/) must be installed and started to
> run the [Ligo](https://ligolang.org/) compiler (and tests).

```shellsession
opam update
opam switch create . ocaml-base-compiler.4.14.0 --deps-only -y
eval $(opam env)
make opam-deps
make opam-dev-deps
```

## Building, testing and pacaking

- `make build` will build the differents smart-contracts into `_buil/contracts_`
- `make test` will run Ligo's tests and tezt-test
- `make package` will produce a _tarball_ of the Michelson version of our contracts.
