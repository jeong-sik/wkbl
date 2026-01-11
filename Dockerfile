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
    pkg-config \
    perl \
    && rm -rf /var/lib/apt/lists/*

USER opam
WORKDIR /home/opam/src

# Update opam repository to find latest dream/caqti versions
RUN opam update

# Copy opam file first to leverage Docker layer caching
COPY --chown=opam:opam ocaml/wkbl.opam .

# Install dependencies (This is the failing step 18)
# Adding --with-test to ensure all potential libs are there, and using -y
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
    ca-certificates \
    netbase \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy binary from build stage
COPY --from=build /home/opam/src/_build/default/bin/main.exe /app/wkbl-server
COPY ocaml/static /app/static

# Expose port
EXPOSE 8080

# Run the server
CMD ["/app/wkbl-server"]