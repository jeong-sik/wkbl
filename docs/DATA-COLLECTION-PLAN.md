# WKBL 공홈 추가 데이터 수집 계획

## 현재 데이터 상태 (2026-01-17 기준)

| 테이블 | 레코드 수 | 완성도 |
|--------|----------|--------|
| `players` | 263 | height 100%, weight 0%, position 100%, birth_date 100% |
| `games` | 1,571 | game_date 100%, attendance 0% |
| `game_stats` | 17,964 | 기본 스탯 100%, +/- 없음 |
| `pbp_events` | 존재 | PBP 데이터 일부 경기 |

---

## Phase 1: 누락 데이터 수집 (즉시 실행 가능)

### 1.1 선수 체중 (Weight) 수집

**현황**: 263명 중 0명 (0%)
**소스**: `https://www.wkbl.or.kr/player/detail2.asp?pcode={player_id}`
**스크립트**: `scripts/scrape_wkbl_players.py` (이미 존재)

**문제 분석**:
- 스크래퍼가 체중 파싱 로직을 가지고 있으나 데이터가 0%
- WKBL 공홈이 체중 정보를 일관되게 제공하지 않을 가능성
- 또는 HTML 구조 변경으로 파싱 실패

**액션**:
```bash
# 테스트 실행 (5명)
python scripts/scrape_wkbl_players.py --limit 5 --dry-run

# 전체 실행
python scripts/scrape_wkbl_players.py
```

### 1.2 경기 관중 수 (Attendance) 수집

**현황**: 1,571경기 중 0경기 (0%)
**소스**: `https://www.wkbl.or.kr/game/result.asp`
**스크립트**: 신규 작성 필요 또는 `scrape_wkbl_games.py` 확장

**HTML 구조** (예상):
```html
<td class="attendance">3,542</td>
```

**스키마 확장**: 이미 `games.attendance` 컬럼 존재 (INTEGER)

**액션**:
1. 경기 결과 페이지에서 관중 수 파싱 로직 추가
2. 백필 스크립트 작성: `scripts/backfill_attendance.py`

---

## Phase 2: 새로운 데이터 수집 (Basketball Reference 스타일)

### 2.1 쿼터별 스코어 테이블

**필요성**: 경기 흐름 분석, Clutch Time 계산
**소스**: 경기 결과 페이지 (boxscore)

**새 테이블**:
```sql
CREATE TABLE IF NOT EXISTS quarter_scores (
    game_id TEXT NOT NULL,
    quarter TEXT NOT NULL,  -- '1Q', '2Q', '3Q', '4Q', 'OT1', 'OT2'
    home_score INTEGER,
    away_score INTEGER,
    PRIMARY KEY (game_id, quarter),
    FOREIGN KEY (game_id) REFERENCES games(game_id)
);
```

### 2.2 선수 +/- 계산

**현황**: PBP 데이터 있음, +/- 계산 안 함
**구현**: PBP 이벤트에서 득점 시점의 온코트 선수 기반 계산

**새 컬럼** (game_stats 테이블):
```sql
ALTER TABLE game_stats ADD COLUMN plus_minus INTEGER;
```

**계산 로직**:
1. PBP 이벤트에서 득점 이벤트 필터링
2. 각 득점 시점의 온코트 5명 추적
3. 해당 선수들에게 +/- 할당

### 2.3 코치 정보

**현황**: `coaches` 테이블 존재 여부 확인 필요
**소스**: `https://www.wkbl.or.kr/team/detail.asp`

**새 테이블** (필요시):
```sql
CREATE TABLE IF NOT EXISTS team_coaches (
    team_code TEXT NOT NULL,
    season_code TEXT NOT NULL,
    coach_name TEXT NOT NULL,
    coach_type TEXT,  -- 'HEAD', 'ASSISTANT'
    PRIMARY KEY (team_code, season_code, coach_name)
);
```

---

## Phase 3: 고급 통계 계산 (서버 사이드)

### 3.1 Usage Rate (USG%)

**공식**:
```
USG% = 100 * ((FGA + 0.44 * FTA + TOV) * (Team MP / 5)) / (MP * (Team FGA + 0.44 * Team FTA + Team TOV))
```

### 3.2 Player Efficiency Rating (PER)

**공식**: Basketball Reference 방식 (복잡)
- 리그 평균 대비 정규화 필요
- 분당 효율성 계산

### 3.3 Win Shares (WS)

**필요 데이터**:
- Offensive Win Shares (OWS)
- Defensive Win Shares (DWS)
- 팀 승률 기반 계산

---

## 우선순위 및 일정

| 순위 | 작업 | 난이도 | 예상 시간 | 의존성 |
|------|------|--------|----------|--------|
| P0 | 선수 체중 수집 | 낮음 | 30분 | - |
| P0 | 관중 수 백필 | 중간 | 2시간 | - |
| P1 | 쿼터 스코어 수집 | 중간 | 3시간 | - |
| P1 | +/- 계산 | 높음 | 4시간 | PBP 완성도 |
| P2 | USG%, PER, WS | 높음 | 1일 | 전체 스탯 |

---

## 데이터 소스 URL 정리

| 데이터 | URL 패턴 | 파라미터 |
|--------|----------|----------|
| 선수 프로필 | `/player/detail2.asp` | `pcode={player_id}` |
| 경기 결과 | `/game/result.asp` | `gyession={season}&game_no={game_no}` |
| 경기 상세 (AJAX) | `/game/ajax/ajax_game_result_2.asp` | POST: `gyession`, `game_no` |
| PBP 데이터 | `/game/pbp.asp` | `gyession={season}&game_no={game_no}` |
| 팀 정보 | `/team/detail.asp` | `tcode={team_code}` |
| 일정 | `/game/sch/schedule1.asp` | `gyession={season}` |

---

## 실행 스크립트 목록

### 기존 스크립트
- `scripts/scrape_wkbl_players.py` - 선수 메타데이터
- `scripts/scrape_wkbl_games.py` - 경기 결과/박스스코어
- `scripts/backfill_game_dates.py` - 경기 날짜 백필
- `scripts/backfill_quarter_scores.py` - 쿼터 스코어 백필

### 신규 작성 필요
- `scripts/backfill_attendance.py` - 관중 수 백필
- `scripts/calculate_plus_minus.py` - +/- 계산
- `scripts/calculate_advanced_stats.py` - 고급 통계

---

## 주의사항

1. **Rate Limiting**: WKBL 서버에 부담 최소화 (요청 간 1초 딜레이)
2. **데이터 검증**: 파싱 결과 sanity check (범위 검사)
3. **증분 업데이트**: 이미 수집된 데이터 스킵 로직
4. **에러 핸들링**: 네트워크 오류 시 재시도 로직
5. **로깅**: 수집 진행 상황 및 오류 기록
