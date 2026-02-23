#!/usr/bin/env bash
# Copy Wasm island build outputs from _build/ to static/wasm/{island}/
# Run after `dune build` to make islands available for serving.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BUILD="${ROOT}/_build/default/apps/api/islands"
DEST="${ROOT}/apps/api/static/wasm"

if [ ! -d "$BUILD" ]; then
  echo "No island build output found. Run 'dune build' first." >&2
  exit 1
fi

for island_dir in "$BUILD"/*/; do
  name="$(basename "$island_dir")"
  js_glue="${island_dir}main.bc.wasm.js"
  assets="${island_dir}main.bc.wasm.assets"

  if [ ! -f "$js_glue" ]; then
    continue
  fi

  target="${DEST}/${name}"
  mkdir -p "$target"
  # Remove existing (dune _build files are read-only)
  rm -f "$target/main.bc.wasm.js"
  cp "$js_glue" "$target/"

  if [ -d "$assets" ]; then
    rm -rf "$target/main.bc.wasm.assets"
    cp -r "$assets" "$target/"
  fi

  echo "Copied island: ${name}"
done
