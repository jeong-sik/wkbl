# 1. Build Stage
FROM ocaml/opam:debian-12-ocaml-5.1 AS build

# Install system dependencies
USER root
RUN apt-get update && apt-get install -y \
    libev-dev \
    libssl-dev \
    libpq-dev \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

USER opam
WORKDIR /home/opam/src

# Copy opam files and install dependencies
COPY --chown=opam:opam ocaml/wkbl.opam .
RUN opam install . --deps-only

# Copy source and build
COPY --chown=opam:opam ocaml/ .
RUN opam exec -- dune build --profile=release bin/main.exe

# 2. Run Stage
FROM debian:12-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libev4 \
    libssl3 \
    libpq5 \
    ca-certificates \
    netbase \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy binary from build stage
COPY --from=build /home/opam/src/_build/default/bin/main.exe /app/wkbl-server
# Copy static assets
COPY ocaml/static /app/static

# Expose port (Dream default or PORT env)
EXPOSE 8080

# Run the server
CMD ["/app/wkbl-server"]
