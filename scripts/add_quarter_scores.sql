-- Add quarter scores to games table
-- Run: sqlite3 data/wkbl.db < scripts/add_quarter_scores.sql

-- Home team quarter scores
ALTER TABLE games ADD COLUMN q1_home INTEGER;
ALTER TABLE games ADD COLUMN q2_home INTEGER;
ALTER TABLE games ADD COLUMN q3_home INTEGER;
ALTER TABLE games ADD COLUMN q4_home INTEGER;
ALTER TABLE games ADD COLUMN ot_home INTEGER DEFAULT 0;

-- Away team quarter scores
ALTER TABLE games ADD COLUMN q1_away INTEGER;
ALTER TABLE games ADD COLUMN q2_away INTEGER;
ALTER TABLE games ADD COLUMN q3_away INTEGER;
ALTER TABLE games ADD COLUMN q4_away INTEGER;
ALTER TABLE games ADD COLUMN ot_away INTEGER DEFAULT 0;

-- Create index for quarter score queries
CREATE INDEX IF NOT EXISTS idx_games_quarters ON games(q1_home, q2_home, q3_home, q4_home);
