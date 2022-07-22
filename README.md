# Chusai

# Developer instructions

## Prerequisites

> **Warning** 
> [Docker](https://www.docker.com/) must be installed and started to
> run the [Ligo](https://ligolang.org/) compiler (and tests).

```shellsession
opam update
opam switch create . ocaml-base-compiler.4.12.1 --deps-only -y
eval $(opam env)
make opam-deps
make opam-dev-deps
```

## Building
```
$ make build
```

## Testing

> **Warning** When the prerequisites are met, you must first provide the
> `tezos-client` and the `tezos-node` to run the integration tests (via `tezt`),
> for this you can use the command: `make get-tezos-binaries` and your node
> and your node must be correctly installed. For this you can refer 
> to [this page](https://tezos.gitlab.io/introduction/howtoget.html#install-rust)

```shellsession
make test # run the full suite (layer1, layer2 and tezt)
make test-layer1 # run only Ligo test
make test-layer2 # run only unit test for layer2 (ocaml part)
```


## Running the metrics
This will print on stdout the existing metrics 

```shellsession
make metrics
```
