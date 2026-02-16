#!/usr/bin/env bash
# Build Tailwind CSS from source (replaces the ~300KB CDN play script)
# Usage: cd ocaml && bash build-css.sh
set -euo pipefail
cd "$(dirname "$0")"
tailwindcss -i input.css -o static/css/tailwind.css --minify
echo "Built static/css/tailwind.css ($(wc -c < static/css/tailwind.css | tr -d ' ') bytes)"
