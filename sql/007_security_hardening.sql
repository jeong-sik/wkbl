-- Supabase Security Advisor hardening for wkbl.
-- This project uses direct server-side Postgres access, so public API roles
-- should not have implicit read access to public tables or definer views.

DO $$
DECLARE
  table_name text;
BEGIN
  FOREACH table_name IN ARRAY ARRAY[
    'awards',
    'coaches',
    'game_stats',
    'game_stats_exclusions',
    'games',
    'historical_milestones',
    'historical_seasons',
    'legend_players',
    'official_trade_events',
    'play_by_play_events',
    'player_career_history',
    'player_drafts',
    'player_external_links',
    'player_plus_minus',
    'players',
    'schedule',
    'seasons',
    'teams'
  ]
  LOOP
    EXECUTE format('ALTER TABLE IF EXISTS %I ENABLE ROW LEVEL SECURITY', table_name);
  END LOOP;
END $$;

ALTER VIEW IF EXISTS player_identities SET (security_invoker = true);
ALTER VIEW IF EXISTS game_stats_clean SET (security_invoker = true);
ALTER VIEW IF EXISTS games_calc SET (security_invoker = true);
