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
    nodejs \
    npm \
    && npm install -g tailwindcss \
    && rm -rf /var/lib/apt/lists/*

USER opam
WORKDIR /home/opam/src/ocaml

# Update opam repository
RUN opam update

# Cache bust for kirin installation
ARG CACHEBUST=2026022001

# Pin private dependencies from GitHub
RUN opam pin add grpc-direct-core https://github.com/jeong-sik/grpc-direct.git#main -y --no-action
RUN opam pin add grpc-direct https://github.com/jeong-sik/grpc-direct.git#main -y --no-action
RUN opam pin add ocaml-webrtc https://github.com/jeong-sik/ocaml-webrtc.git#main -y --no-action
RUN opam pin add kirin https://github.com/jeong-sik/kirin.git#main -y --no-action
RUN opam update grpc-direct-core grpc-direct ocaml-webrtc kirin
RUN opam install kirin -y

# 1. OCaml Build
COPY --chown=opam:opam ocaml/wkbl.opam .
RUN opam install . --deps-only -y
COPY --chown=opam:opam ocaml/ .
# Build CSS
RUN ./build-css.sh
RUN opam exec -- dune build --profile=release bin/main.exe bin/scraper_tool.exe

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
    binutils \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy binaries from build stage
COPY --from=build /home/opam/src/ocaml/_build/default/bin/main.exe /app/wkbl-server
COPY --from=build /home/opam/src/ocaml/_build/default/bin/scraper_tool.exe /app/wkbl-scraper
# Copy static files (from build stage to include generated CSS)
COPY --from=build /home/opam/src/ocaml/static /app/static

# Environment variables
ENV PORT=8080

# Expose port
EXPOSE 8080

# Run the server
CMD ["/app/wkbl-server"]
