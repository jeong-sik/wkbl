# Wasm Islands Architecture

SSR HTML + selective Wasm hydration using `wasm_of_ocaml`.

## How It Works

```
SSR (OCaml server)
  renders <div data-island="table_sort">...</div>
       |
island-loader.js (DOMContentLoaded)
  querySelectorAll('[data-island]:not([data-island-hydrated])')
  loads /static/wasm/{name}/main.bc.wasm.js
       |
htmx:afterSwap
  re-scans swapped subtree for new islands
```

1. Server renders HTML with `data-island="{name}"` attributes
2. `island-loader.js` discovers unhydrated islands on page load
3. Loads Wasm glue script from `/static/wasm/{name}/main.bc.wasm.js`
4. Wasm module initializes, binds to DOM, marks `data-island-hydrated="true"`
5. After HTMX swaps, new islands in the swapped subtree are re-hydrated

Fallback: if Wasm loading fails, `data-island-fallback` attribute specifies a JS script to load instead.

## Directory Structure

```
apps/api/
  island_core/           # Shared pure-computation library (no DOM/IO)
    table_sort_logic.ml  # Sort value parsing and comparison
    number_format.ml     # Human-readable number formatting (1.2K, 3.5만)
    prediction_calc.ml   # Elo, Pythagorean, Log5, blend probabilities
  islands/               # Wasm island executables (DOM bindings)
    table_sort/main.ml   # Sortable table headers
    number_format/main.ml# Format large numbers in-place
    prediction_gauge/main.ml # Interactive prediction breakdown
  static/
    js/island-loader.js  # Island discovery and Wasm loading
    wasm/                # Build output (gitignored)
      table_sort/main.bc.wasm.js
      number_format/main.bc.wasm.js
      prediction_gauge/main.bc.wasm.js
  test/
    test_island_core.ml  # 27 Alcotest tests for island_core modules
```

## Islands

| Island | Shared Module | JS Fallback | Description |
|--------|--------------|-------------|-------------|
| table_sort | table_sort_logic.ml | table-sort.js | Click-to-sort table columns |
| number_format | number_format.ml | number-format.js | Format numbers (1234 to 1.2K) |
| prediction_gauge | prediction_calc.ml | (none) | Interactive prediction breakdown |

## Shared Library: wkbl_island_core

Pure OCaml computation shared between server and Wasm islands.
No js_of_ocaml DOM dependencies in the core modules themselves.

```
wkbl (server: kirin, caqti, eio)
  depends on -> wkbl_island_core

islands/table_sort/main.ml
  depends on -> wkbl_island_core + js_of_ocaml
```

Server reuses the same logic for SSR; islands reuse it for client-side interactivity.

## Building

```bash
# Build everything (server + islands)
dune build --root .

# Build islands only
dune build --root . islands/

# Copy Wasm output to static/ for local dev
scripts/copy-wasm-islands.sh

# Run tests (includes island_core tests)
dune runtest --root .
```

Docker builds handle Wasm compilation and static file copying automatically (see Dockerfile lines 42-53).

## Adding a New Island

1. Create `island_core/{module}.ml` with pure computation logic
2. Create `islands/{name}/main.ml` with DOM bindings using `js_of_ocaml`
3. Create `islands/{name}/dune`:
   ```dune
   (executable
    (name main)
    (modes wasm)
    (libraries wkbl_island_core js_of_ocaml)
    (preprocess (pps js_of_ocaml-ppx))
    (wasm_of_ocaml (compilation_mode separate)))
   ```
4. Add `data-island="{name}"` to the relevant view
5. Add tests to `test/test_island_core.ml`
6. Run `scripts/copy-wasm-islands.sh` for local dev

## Technical Notes

- **wasm_of_ocaml 6.3**: Compiles OCaml bytecode to WebAssembly
- **Dune 3.17**: Required for `(modes wasm)` build target
- **Compilation modes**: `separate` for dev (faster), `whole_program` for release
- **Fallback**: `data-island-fallback="/static/js/{name}.js"` attribute on the island container
- **HTMX**: Islands re-hydrate after `htmx:afterSwap` events on swapped subtrees
