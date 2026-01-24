# 1. Build Stage
FROM ocaml/opam:debian-12-ocaml-5.1 AS build

# Install ALL necessary system dependencies for Dream and Caqti
USER root
RUN apt-get update && apt-get install -y \
    libev-dev \
    libssl-dev \
    libpq-dev \
    libgmp-dev \
    libffi-dev \
    libsqlite3-dev \
    liburing-dev \
    pkg-config \
    perl \
    && rm -rf /var/lib/apt/lists/*

USER opam
WORKDIR /home/opam/src/ocaml

# Update opam repository to find latest dream/caqti versions
RUN opam update

# Cache bust for kirin installation (change this value to force rebuild)
ARG CACHEBUST=2026012411

# Pin private dependencies from GitHub (not in opam)
# grpc-direct repo has multiple packages: grpc-direct-core and grpc-direct
# Both must be pinned explicitly for opam to find them
RUN opam pin add grpc-direct-core https://github.com/jeong-sik/grpc-direct.git#main -y --no-action
RUN opam pin add grpc-direct https://github.com/jeong-sik/grpc-direct.git#main -y --no-action
RUN opam pin add ocaml-webrtc https://github.com/jeong-sik/ocaml-webrtc.git#main -y --no-action
RUN opam pin add kirin https://github.com/jeong-sik/kirin.git#main -y --no-action
# Force fetch latest commits from pinned repos (opam caches git repos)
RUN opam update grpc-direct-core grpc-direct ocaml-webrtc kirin
RUN opam install kirin -y

# Copy opam file first to leverage Docker layer caching
COPY --chown=opam:opam ocaml/wkbl.opam .

# Install dependencies
RUN opam install . --deps-only -y

# Copy source and build
COPY --chown=opam:opam ocaml/ .
RUN opam exec -- dune build --profile=release bin/main.exe

# 2. Run Stage
FROM debian:12-slim

# Install ONLY runtime libraries
RUN apt-get update && apt-get install -y \
    libev4 \
    libssl3 \
    libpq5 \
    libgmp10 \
    libsqlite3-0 \
    ca-certificates \
    netbase \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy binary from build stage
COPY --from=build /home/opam/src/ocaml/_build/default/bin/main.exe /app/wkbl-server
COPY ocaml/static /app/static

# Expose port
EXPOSE 8080

# Run the server
CMD ["/app/wkbl-server"]
