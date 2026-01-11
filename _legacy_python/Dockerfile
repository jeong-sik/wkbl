# syntax=docker/dockerfile:1

FROM python:3.11-slim AS data-builder

ARG WKBL_YEARS=5
ENV PIP_DISABLE_PIP_VERSION_CHECK=1 \
    PIP_NO_CACHE_DIR=1

WORKDIR /app

COPY requirements.txt .
RUN python -m pip install -r requirements.txt

COPY scripts/ scripts/

# Fetch -> aggregate -> build SQLite DB (data/wkbl.db).
RUN python scripts/wkbl_refresh_all.py --skip-qa -- --years ${WKBL_YEARS}
RUN python scripts/build_wkbl_db.py --force


FROM ocaml/opam:debian-12-ocaml-5.1 AS ocaml-builder

WORKDIR /app

# Caqti SQLite driver requires sqlite3 headers.
RUN sudo apt-get update \
  && sudo apt-get install -y --no-install-recommends libsqlite3-dev \
  && sudo rm -rf /var/lib/apt/lists/*

COPY ocaml/ ocaml/
WORKDIR /app/ocaml

RUN opam install . --deps-only -y
RUN opam exec -- dune build --profile=release


FROM debian:bookworm-slim AS runtime

RUN apt-get update \
  && apt-get install -y --no-install-recommends ca-certificates libsqlite3-0 \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=data-builder /app/data/wkbl.db /app/data/wkbl.db
COPY --from=ocaml-builder /app/ocaml/_build/default/bin/main.exe /app/wkbl
COPY ocaml/static/ /app/static/

ENV WKBL_DB_PATH=/app/data/wkbl.db \
    WKBL_STATIC_PATH=/app/static

EXPOSE 8000

CMD ["/app/wkbl"]

