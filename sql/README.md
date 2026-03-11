# WKBL Database (Supabase)

## Connection Info

| Item | Value |
|------|-------|
| Provider | Supabase (AWS ap-southeast-1) |
| Project ID | `efgbkvmwwefqjxeugktf` |
| Host | `***REMOVED***` |
| Port | `6543` (Pooler) |
| Database | `postgres` |
| Dashboard | https://supabase.com/dashboard/project/efgbkvmwwefqjxeugktf |

## Environment Variable

```bash
# ~/.zshenv
export WKBL_DATABASE_URL="postgresql://***REMOVED***:***@***REMOVED***:6543/postgres"

# Railway (wkbl_server)
WKBL_DATABASE_URL (same as above)
```

## Schema Files

| File | Description |
|------|-------------|
| `000_schema_backup.sql` | Full DDL backup (pg_dump --schema-only) |
| `004_history_tables.sql` | History tables (historical_seasons, legend_players, etc.) |
| `005_history_data.sql` | History seed data |
| `006_qa_indexes.sql` | QA performance indexes (schedule ↔ games joins) |
| `007_security_hardening.sql` | Supabase Security Advisor hardening (RLS + security_invoker) |

## Main Tables

### Core Data
- `teams` - 6 WKBL teams
- `players` - All players (current & historical)
- `games` - Game schedule & results
- `boxscores` - Player stats per game

### History Data
- `historical_seasons` - Season champions, MVPs (1993~)
- `legend_players` - Hall of fame players
- `coaches` - Coaches history
- `player_career_history` - Player stats by season

## Quick Access

```bash
# Connect via psql
psql "$WKBL_DATABASE_URL"

# Check tables
psql "$WKBL_DATABASE_URL" -c "\dt"

# Backup DDL
pg_dump "$WKBL_DATABASE_URL" --schema-only > backup.sql

# Apply Security Advisor hardening
psql "$WKBL_DATABASE_URL" -f sql/007_security_hardening.sql
```

## Last Updated
- Schema backup: 2026-01-17
