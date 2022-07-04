# Chusai

# Developer instructions
## Prerequisites
You need to have docker installed. This is used to run the Ligo compiler and a local switch:

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
```
$ make test
```

## Running the metrics
This will print on stdout the existing metrics 

```
$ make metrics
```
