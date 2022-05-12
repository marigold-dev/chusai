# Chusai

## Developer instructions

### Prerequisites

You need to have docker installed. This is used to run the Ligo compiler

## Development environment

```shellsession
$ opam update
$ opam switch create . ocaml-base-compiler.4.14.0 --deps-only -y
$ eval $(opam env)
```

### Building

```shellsession
$ make build
```

### Testing

```shellsession
$ make test
```

### Running the metrics
This will print on stdout the existing metrics 

```shellsession
$ make metrics
```
