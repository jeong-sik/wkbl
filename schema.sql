-- WKBL Database Schema
-- Optimized for basketball-reference style analysis

-- 1. Seasons (시즌 정보)
CREATE TABLE seasons (
    season_code TEXT PRIMARY KEY, -- ex: '046'
    season_name TEXT NOT NULL,    -- ex: '2025-2026'
    start_date DATE,
    end_date DATE
);

-- 2. Teams (팀 정보)
CREATE TABLE teams (
    team_code TEXT PRIMARY KEY,   -- ex: '05'
    team_name_kr TEXT NOT NULL,   -- ex: '우리은행'
    team_name_en TEXT,            -- ex: 'WOORI BANK'
    home_stadium TEXT
);

-- 3. Players (선수 정보)
CREATE TABLE players (
    player_id TEXT PRIMARY KEY,   -- WKBL ID ex: '095703'
    player_name TEXT NOT NULL,
    position TEXT,                -- G, F, C
    birth_date DATE,
    height INTEGER,
    weight INTEGER
);

-- 4. Games (경기 정보)
CREATE TABLE games (
    game_id TEXT PRIMARY KEY,     -- ex: '046-01-33'
    season_code TEXT REFERENCES seasons(season_code),
    game_type TEXT NOT NULL,       -- '01': 정규리그, '02': 플레이오프
    game_no INTEGER NOT NULL,
    game_date DATE,
    home_team_code TEXT REFERENCES teams(team_code),
    away_team_code TEXT REFERENCES teams(team_code),
    home_score INTEGER,
    away_score INTEGER,
    stadium TEXT,
    attendance INTEGER
);

-- 5. Game Stats (경기 기록 - Box Score)
CREATE TABLE game_stats (
    id SERIAL PRIMARY KEY,
    game_id TEXT REFERENCES games(game_id),
    team_code TEXT REFERENCES teams(team_code),
    player_id TEXT REFERENCES players(player_id),
    
    -- Basic Stats
    min_seconds INTEGER,
    fg_2p_m INTEGER DEFAULT 0,
    fg_2p_a INTEGER DEFAULT 0,
    fg_3p_m INTEGER DEFAULT 0,
    fg_3p_a INTEGER DEFAULT 0,
    ft_m INTEGER DEFAULT 0,
    ft_a INTEGER DEFAULT 0,
    
    reb_off INTEGER DEFAULT 0,
    reb_def INTEGER DEFAULT 0,
    reb_tot INTEGER DEFAULT 0,
    
    ast INTEGER DEFAULT 0,
    stl INTEGER DEFAULT 0,
    blk INTEGER DEFAULT 0,
    tov INTEGER DEFAULT 0,
    pf INTEGER DEFAULT 0,
    pts INTEGER DEFAULT 0,
    
    -- Advanced Stats (Calculated)
    ts_pct REAL,
    efg_pct REAL,
    game_score REAL,
    
    UNIQUE(game_id, player_id)
);

-- Indexes
CREATE INDEX idx_game_stats_player ON game_stats(player_id);
CREATE INDEX idx_game_stats_game ON game_stats(game_id);
CREATE INDEX idx_games_date ON games(game_date);
