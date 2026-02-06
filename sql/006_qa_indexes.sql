-- QA performance indexes (schedule ↔ games joins)

-- Completed schedule lookups
CREATE INDEX IF NOT EXISTS idx_schedule_completed_keys
  ON schedule (season_code, game_date, home_team_code, away_team_code)
  WHERE status = 'completed';

-- Join key for schedule ↔ games matching
CREATE INDEX IF NOT EXISTS idx_games_season_date_teams
  ON games (season_code, game_date, home_team_code, away_team_code);
