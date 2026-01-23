# WKBL 고급 통계 시스템 설계

## 현재 상태 (2026-01-17)

### 이미 계산된 통계 (game_stats 테이블)
| 통계 | 컬럼 | 상태 |
|------|------|------|
| True Shooting % | `ts_pct` | ✅ 완료 |
| Effective FG % | `efg_pct` | ✅ 완료 |
| Game Score | `game_score` | ✅ 완료 |

### 계산 필요한 통계
| 통계 | 테이블 | 상태 | 난이도 |
|------|--------|------|--------|
| Plus/Minus (+/-) | `player_plus_minus` | 🔴 테이블 있으나 비어있음 | 높음 |
| Usage Rate (USG%) | `game_stats.usg_pct` | 🟡 컬럼 추가 필요 | 낮음 |
| Player Efficiency Rating | `game_stats.per` | 🟡 컬럼 추가 필요 | 중간 |
| Win Shares | 새 테이블 | 🔴 없음 | 높음 |

---

## Phase 1: Usage Rate (USG%) 구현

### 공식 (Basketball Reference)
```
USG% = 100 * ((FGA + 0.44 * FTA + TOV) * (Team MP / 5)) /
       (MP * (Team FGA + 0.44 * Team FTA + Team TOV))
```

### 데이터 가용성 ✅
```sql
-- 선수 데이터 (game_stats)
- min_seconds (MP)
- fg_2p_a + fg_3p_a (FGA)
- ft_a (FTA)
- tov (TOV)

-- 팀 합계 (GROUP BY 집계)
SELECT
  game_id, team_code,
  SUM(min_seconds) as team_mp,
  SUM(fg_2p_a + fg_3p_a) as team_fga,
  SUM(ft_a) as team_fta,
  SUM(tov) as team_tov
FROM game_stats
GROUP BY game_id, team_code
```

### 구현 방법

#### Option A: SQL View (추천)
```sql
CREATE OR REPLACE VIEW player_advanced_stats AS
WITH team_totals AS (
  SELECT
    game_id, team_code,
    SUM(min_seconds) as team_mp,
    SUM(fg_2p_a + fg_3p_a) as team_fga,
    SUM(ft_a) as team_fta,
    SUM(tov) as team_tov
  FROM game_stats
  WHERE min_seconds > 0
  GROUP BY game_id, team_code
)
SELECT
  gs.*,
  CASE
    WHEN gs.min_seconds > 0 AND tt.team_fga + 0.44 * tt.team_fta + tt.team_tov > 0 THEN
      ROUND(100.0 * (
        (gs.fg_2p_a + gs.fg_3p_a + 0.44 * gs.ft_a + gs.tov) * (tt.team_mp / 5.0)
      ) / (
        gs.min_seconds * (tt.team_fga + 0.44 * tt.team_fta + tt.team_tov)
      ), 1)
    ELSE NULL
  END as usg_pct
FROM game_stats gs
JOIN team_totals tt ON gs.game_id = tt.game_id AND gs.team_code = tt.team_code;
```

#### Option B: 컬럼 추가 + 백필 스크립트
```sql
ALTER TABLE game_stats ADD COLUMN usg_pct REAL;

-- 백필 스크립트 (Python)
-- scripts/calculate_usg_pct.py
```

---

## Phase 2: Plus/Minus (+/-) 구현

### 현재 상태
- `player_plus_minus` 테이블 존재 (비어있음)
- `play_by_play_events` 테이블: 674,195 이벤트, 1,535 경기

### PBP 데이터 구조
```
id | game_id | period_code | event_index | team_side | description | team1_score | team2_score | clock
```

### 과제: On-Court 선수 추적
PBP 데이터에 "누가 코트에 있는지" 직접 기록이 없음.

#### 해결 방안 1: 교체 이벤트 파싱
```python
# description 패턴 분석
"박혜진 교체 OUT"
"김단비 교체 IN"
```

#### 해결 방안 2: 이벤트 참여자 기반 추론
```python
# 득점/리바운드/어시스트 등 이벤트에서 선수 추출
"박혜진 2점슛성공" → 박혜진 on court
"양지희 블록" → 양지희 on court
```

### 구현 계획
1. PBP description에서 교체(IN/OUT) 이벤트 파싱
2. 각 시점의 on-court 5명 추적
3. 득점 발생 시 해당 5명에게 +/- 할당
4. `player_plus_minus` 테이블에 저장

### 필요 스크립트
```bash
scripts/calculate_plus_minus.py
  - PBP 이벤트 파싱
  - 온코트 라인업 추적
  - +/- 계산 및 저장
```

---

## Phase 3: Player Efficiency Rating (PER)

### 공식 (간소화 버전)
```
PER = (1/MP) * [
  + 3P Made
  + (2/3) * Assists
  + (2 - factor * (team_AST / team_FG)) * FG Made
  + (FT Made * 0.5 * (1 + (1 - (team_AST / team_FG)) + (2/3) * (team_AST / team_FG)))
  - VOP * Turnovers
  - VOP * DRB% * (FGA - FG)
  - VOP * 0.44 * (0.44 + (0.56 * DRB%)) * (FTA - FT)
  + VOP * (1 - DRB%) * (TRB - ORB)
  + VOP * DRB% * ORB
  + VOP * Steals
  + VOP * DRB% * Blocks
  - Personal Fouls * ((lg_FT / lg_PF) - 0.44 * (lg_FTA / lg_PF) * VOP)
]
```

### 리그 평균 계산 필요
- lg_Pace: 리그 평균 페이스
- VOP: Value of Possession
- DRB%: Defensive Rebound Percentage
- factor: 어시스트 팩터

### 구현 우선순위: 낮음
- USG%와 +/- 완료 후 진행
- 리그 평균 데이터 집계 필요

---

## Phase 4: Clutch Time Stats

### 정의
- **Clutch Time**: Q4 + 점수차 5점 이내 + 5분 미만 남음

### PBP 기반 필터링
```sql
SELECT *
FROM play_by_play_events
WHERE period_code = 'Q4'
  AND ABS(team1_score - team2_score) <= 5
  AND clock LIKE '__:__'  -- 파싱 필요
  AND CAST(SUBSTRING(clock, 1, 2) AS INT) < 5
```

### 새 뷰/테이블
```sql
CREATE VIEW clutch_stats AS
-- 클러치 타임 이벤트에서 선수별 스탯 집계
```

---

## 구현 로드맵

| 순서 | 기능 | 예상 시간 | 의존성 |
|------|------|----------|--------|
| 1 | USG% View 생성 | 30분 | 없음 |
| 2 | Leaders 페이지에 USG% 추가 | 1시간 | #1 |
| 3 | PBP 교체 이벤트 파싱 | 2시간 | 없음 |
| 4 | +/- 계산 스크립트 | 3시간 | #3 |
| 5 | Clutch 필터 구현 | 2시간 | 없음 |
| 6 | PER 계산 | 4시간 | #1 |

---

## API 엔드포인트 설계

```
GET /leaders?cat=usg       # USG% 리더보드
GET /leaders?cat=plusminus # +/- 리더보드
GET /leaders?cat=clutch    # 클러치 스탯
GET /player/:id/advanced   # 선수별 고급 스탯
GET /game/:id/advanced     # 경기별 고급 스탯
```

---

## OCaml 구현 파일

| 파일 | 역할 |
|------|------|
| `lib/advanced_stats.ml` | USG%, PER 계산 로직 |
| `lib/clutch.ml` | 클러치 타임 필터 |
| `lib/plus_minus.ml` | +/- 조회 (계산은 Python) |
| `lib/db.ml` | 새 쿼리 추가 |
| `lib/views.ml` | 새 UI 컴포넌트 |

---

## 즉시 실행 가능한 Quick Win

### 1. USG% View 생성 (5분)
```sql
CREATE OR REPLACE VIEW player_usg_stats AS
WITH team_totals AS (
  SELECT game_id, team_code,
    SUM(min_seconds) as team_mp,
    SUM(fg_2p_a + fg_3p_a) as team_fga,
    SUM(ft_a) as team_fta,
    SUM(tov) as team_tov
  FROM game_stats WHERE min_seconds > 0
  GROUP BY game_id, team_code
)
SELECT
  gs.game_id, gs.player_id, gs.team_code,
  gs.min_seconds, gs.pts,
  CASE WHEN gs.min_seconds > 0 AND (tt.team_fga + 0.44 * tt.team_fta + tt.team_tov) > 0
  THEN ROUND(100.0 * ((gs.fg_2p_a + gs.fg_3p_a + 0.44 * gs.ft_a + gs.tov) * (tt.team_mp / 5.0)) /
       (gs.min_seconds * (tt.team_fga + 0.44 * tt.team_fta + tt.team_tov)), 1)
  ELSE NULL END as usg_pct
FROM game_stats gs
JOIN team_totals tt ON gs.game_id = tt.game_id AND gs.team_code = tt.team_code;
```

### 2. 시즌 USG% 리더 조회
```sql
SELECT p.player_name,
  ROUND(AVG(u.usg_pct), 1) as avg_usg,
  COUNT(*) as games
FROM player_usg_stats u
JOIN players p ON u.player_id = p.player_id
WHERE u.game_id LIKE '046-%'  -- 현 시즌
GROUP BY p.player_id, p.player_name
HAVING COUNT(*) >= 10
ORDER BY avg_usg DESC
LIMIT 10;
```
