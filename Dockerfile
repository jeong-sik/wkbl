# 1. Build Stage
FROM ocaml/opam:debian-12-ocaml-5.1 AS build

# Install ALL necessary system dependencies for Dream, Caqti, and Python (for data pipeline)
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
    python3 \
    python3-pip \
    python3-venv \
    && rm -rf /var/lib/apt/lists/*

USER opam
WORKDIR /home/opam/src/ocaml

# Update opam repository
RUN opam update

# Cache bust for kirin installation
ARG CACHEBUST=2026012411

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
RUN opam exec -- dune build --profile=release bin/main.exe

# 2. Python Data Pipeline (Build DB)
# Copy scripts and requirements to a separate dir to avoid messing up ocaml build ctx if possible,
# but here we just use a subfolder for clarity.
WORKDIR /home/opam/src/data-pipeline

COPY requirements.txt .
# Create venv and install dependencies
RUN python3 -m venv venv && \
    ./venv/bin/pip install --no-cache-dir -r requirements.txt

# Copy scripts and data folder structure
COPY scripts/ ./scripts/
COPY data/ ./data/

# Run the full refresh pipeline to generate wkbl.db
# We assume this creates data/wkbl.db
RUN ./venv/bin/python3 scripts/wkbl_refresh_all.py --skip-qa

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

# Copy binary from build stage
COPY --from=build /home/opam/src/ocaml/_build/default/bin/main.exe /app/wkbl-server
COPY ocaml/static /app/static

# Copy generated DB from build stage
COPY --from=build /home/opam/src/data-pipeline/data/wkbl.db /app/data/wkbl.db

# Environment variables
ENV WKBL_DB_PATH=/app/data/wkbl.db
ENV PORT=8080

# Expose port
EXPOSE 8080

# Run the server
CMD ["/app/wkbl-server"]
