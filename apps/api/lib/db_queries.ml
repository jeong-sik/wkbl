(** SQL query definitions — re-export facade.

    Original monolithic module split into:
    - Db_queries_schema: DDL (CREATE TABLE/VIEW/INDEX, DROP, REFRESH)
    - Db_queries_seeds: Seed data (INSERT ... ON CONFLICT)
    - Db_queries_core: Basic CRUD, lookups, upserts
    - Db_queries_stats: Leaders, advanced stats, MVP/clutch
    - Db_queries_qa: QA/data quality queries
    - Db_queries_awards: Award-related queries

    All queries use oneshot mode via Db_request for PgBouncer compatibility.
*)

include Db_queries_schema
include Db_queries_seeds
include Db_queries_core
include Db_queries_stats
include Db_queries_qa
include Db_queries_awards
