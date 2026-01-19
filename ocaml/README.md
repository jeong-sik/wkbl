# wkbl-ocaml

WKBL analytics service implemented in OCaml (Dream + Caqti).

## Requirements

- OCaml 5.1+
- opam
- SQLite/PostgreSQL client libraries (for Caqti drivers)

## Setup

```bash
opam install . --deps-only --with-test -y
```

## Build

```bash
dune build
```

## Test

```bash
dune runtest
```

## Run

```bash
dune exec -- wkbl
```
