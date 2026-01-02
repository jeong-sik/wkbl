# WKBL Data Quality Report

**Generated**: 2026-01-02
**Analyst**: data-analyst (MASC)
**Database**: `data/wkbl.db`

---

## Executive Summary

| Metric | Value | Status |
|--------|-------|--------|
| Total Players | 129 | OK |
| Total Games | 200 | OK |
| Total Game Stats | 3,976 | OK |
| **Data Completeness** | **~40%** | CRITICAL |

**Critical Finding**: User reported "숫자가 비현실적으로 안맞음" (unrealistic numbers).
After analysis, the **game statistics are mathematically consistent** but **metadata is severely incomplete**.

---

## Issue #1: Player Metadata 100% NULL (CRITICAL)

```
Table: players (129 rows)
- height: 129 NULL (100%)
- weight: 129 NULL (100%)
- birth_date: 129 NULL (100%)
- position: 129 NULL (100%)
```

**Impact**: Cannot calculate age-based stats, height/position analysis, BMI comparisons.

**Root Cause**: Raw crawled data (`advanced_stats.json`) contains `position` field, but it's not being extracted to the database during ETL.

**Fix Required**:
1. Update ETL to extract position from boxscore data
2. Scrape player profile pages from WKBL.OR.KR for height/weight/birth_date

---

## Issue #2: Game Dates & Attendance 100% NULL (CRITICAL)

```
Table: games (200 rows)
- game_date: 200 NULL (100%)
- attendance: 200 NULL (100%)
- home_score: 2 NULL (1%)
- away_score: 2 NULL (1%)
```

**Impact**: Cannot do time-series analysis, rest day calculations, home/away performance by date.

**Root Cause**: Game date not being parsed from raw HTML or API response.

**Fix Required**:
1. Parse `game_date` from games list HTML/API
2. Attendance data may require separate scraping from game result pages

---

## Issue #3: Missing Team Metadata

```
Table: teams (10 rows)
- team_name_en: 10 NULL (100%)
- home_stadium: 10 NULL (100%)
```

**Impact**: Missing English names for international use, stadium analysis.

---

## Issue #4: Missing Season Dates

```
Table: seasons (3 rows)
- start_date: 3 NULL (100%)
- end_date: 3 NULL (100%)
```

---

## Data That IS Correct (Good News)

### 1. Game Stats Mathematical Consistency
- Points calculation: `pts = fg_2p_m*2 + fg_3p_m*3 + ft_m` - **100% match (3,976/3,976)**
- Rebounds: `reb_tot = reb_off + reb_def` - **100% match**
- Team totals match game scores - **Verified**

### 2. Stats Distribution (Realistic)

| Stat | Min | Max | Avg |
|------|-----|-----|-----|
| pts | 0 | 42 | 6.3 |
| reb_tot | 0 | 24 | 3.5 |
| ast | 0 | 14 | 1.6 |
| stl | 0 | 7 | 0.7 |
| blk | 0 | 5 | 0.3 |
| tov | 0 | 9 | 1.1 |
| pf | 0 | 5 | 1.9 |

All within realistic WKBL ranges.

### 3. Shooting Percentages (Reasonable)
Top FG2%: 50-60% (realistic for WKBL centers)
Top FG3%: 25-35% (realistic for WKBL guards)
FT%: 60-85% range (normal distribution)

### 4. Advanced Stats Calculated
- ts_pct, efg_pct, game_score: **0 NULL (all calculated)**

### 5. Top Players Look Correct
| Player | PPG | RPG | APG | MPG |
|--------|-----|-----|-----|-----|
| 김단비 | 19.2 | 10.3 | 4.1 | 35.3 |
| 박지수 | 18.9 | 13.5 | 4.6 | 27.1 |
| 김소니아 | 16.0 | 9.3 | 2.7 | 33.9 |

These match expected WKBL star player stats.

---

## Action Items

### Priority 1: Fix Player Metadata (Effort: Medium)
1. Extract `position` from existing crawled boxscore data
2. Scrape WKBL player profile pages: `https://www.wkbl.or.kr/player/profile.asp?pcode=XXXXXX`
   - Height, weight, birth_date available there

### Priority 2: Fix Game Dates (Effort: Medium)
1. Parse game_date from games list response
2. Update games table with dates from raw data

### Priority 3: Enrich Team/Season Data (Effort: Low)
1. Manually add team_name_en and home_stadium (only 6 real teams)
2. Add season start/end dates (known from WKBL schedule)

---

## Data Sources for Enrichment

| Data | Source | Method |
|------|--------|--------|
| Player height/weight/birth | wkbl.or.kr player profiles | HTTP scrape |
| Game dates | wkbl.or.kr game schedule | Existing crawl data |
| Team stadiums | Public knowledge | Manual entry |
| Attendance | wkbl.or.kr game results | HTTP scrape |

---

## Conclusion

The **game statistics are mathematically correct and realistic**. The issue is **missing metadata**, not incorrect numbers.

"숫자가 안맞음" likely refers to:
1. Missing player age/height for context
2. Missing game dates for time-series
3. Dashboard showing NULL values instead of real data

**Recommended Next Step**: Run ETL update to extract position from existing raw data, then scrape player profiles for remaining metadata.
