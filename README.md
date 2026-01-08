# WKBL Moneyball Lab

WKBL 시즌 박스스코어를 수집해 basketball-reference 스타일로 탐색/비교하는 대시보드입니다. **OCaml (Dream + HTMX)** 엔진이 기본 웹 서버입니다.

## 아키텍처
- **Web Server (Main)**: `ocaml/` (Dream + HTMX + SQLite) - 고성능 통계 엔진 및 UI
- **Web Server (Legacy)**: `app/` (FastAPI + Jinja2)
- **Data Pipeline**: `scripts/` (Python) - 데이터 수집, 집계, SQLite DB 생성
- **Database**: `data/wkbl.db` (SQLite)

## 빠른 시작 (OCaml 엔진 실행)

### 1. 데이터 준비 (로컬)
데이터가 없다면 먼저 수집 및 DB 생성이 필요합니다.
```bash
# Python 의존성 설치
pip install -r requirements.txt

# 전체 갱신 파이프라인 (수집 -> 집계 -> DB 생성)
python3 scripts/wkbl_refresh_all.py -- --years 5
```

### 2. OCaml 엔진 빌드 및 실행
```bash
cd ocaml
dune build
dune exec wkbl  # 포트 8000에서 실행
```

## 배포 (상시) - Docker
이 레포는 루트의 `Dockerfile`로 바로 컨테이너 배포할 수 있습니다.

### 로컬에서 Docker로 실행
```bash
# (빌드 시) WKBL 공식 사이트에서 데이터 수집/집계 후 wkbl.db를 생성합니다.
docker build -t wkbl .

docker run --rm -p 8000:8000 wkbl
```

### Railway에 상시 배포 (추천)
1. Railway에서 GitHub repo를 연결하고, Dockerfile 기반으로 서비스를 생성합니다.
2. Health check는 `/health`를 사용합니다.
3. 데이터는 이미지 빌드 단계에서 생성되므로, 최신 데이터가 필요하면 재배포(redeploy)로 갱신합니다.

### VPS + Cloudflare Tunnel (월 ~$5, 덜 귀찮은 상시 운영)
- `docs/DEPLOY-VPS.md` 참고

#### 환경변수
- `PORT`: 플랫폼이 주입 (없으면 8000)
- `WKBL_DB_PATH`: SQLite 경로 (기본값: `data/wkbl.db`)
- `WKBL_STATIC_PATH`: 정적 파일 경로 (기본값: 자동 탐색)
- `CF_WEB_ANALYTICS_TOKEN`: Cloudflare Web Analytics token (설정 시 자동 삽입)

## 엔드포인트 (OCaml)
- `/`: 홈 (선수 목록)
- `/players`: 선수 집계 (검색/정렬/HTMX)
- `/teams`: 팀 집계 (시즌/범위 필터)
- `/teams/:team`: 팀별 선수 상세
- `/qa`: 데이터 품질 보고서
- `/health`: 헬스체크

## 데이터 수집 및 관리 (Python)

### 갱신 파이프라인 옵션
```bash
# 10년치 백필
python3 scripts/wkbl_refresh_all.py -- --years 10

# 공식 Draft/Trade 동기화 (네트워크 필요)
python3 scripts/wkbl_refresh_all.py --sync-draft-trade
# 또는 (DB 백업 포함)
python3 scripts/wkbl_draft_trade_sync.py --only-missing --backup

# 선수 외부 링크(인스타/영상) 동기화 (로컬 JSON, 출처 포함)
python3 scripts/wkbl_refresh_all.py --sync-player-links --skip-collect --skip-aggregate --skip-qa
# 또는 (DB 백업 포함)
python3 scripts/wkbl_player_links_sync.py --backup

# 로스터 및 사진 검증 포함
python3 scripts/wkbl_refresh_all.py --update-roster --audit-photos
```

### 개별 스크립트
- `scripts/wkbl_ajax_collector.py`: 공식 사이트 AJAX 데이터 수집
- `scripts/wkbl_draft_trade_sync.py`: 공식 Draft/Trade(드래프트/이적현황) 동기화
- `scripts/wkbl_player_links_sync.py`: 선수 외부 링크(인스타/영상) 동기화 (수동 JSON 기반)
- `scripts/wkbl_aggregate_stats.py`: 수집된 CSV 집계
- `scripts/build_wkbl_db.py`: 집계된 CSV를 SQLite(`wkbl.db`)로 변환
- `scripts/wkbl_db_dedupe_game_stats.py`: 동명이인 등으로 생긴 중복 game_stats 정리
- `scripts/wkbl_data_qa.py`: 데이터 무결성 체크

### Draft/Trade 데이터 신뢰도
- Draft는 `player_id`(pno) 기반으로 공식 선수 상세 페이지에서 가져와 신뢰도가 높습니다.
- Trade(이적 이벤트)는 공식 이적현황 원문을 저장하고, **선수 프로필에서는 "이름 포함"으로만 매칭**하므로 동명이인/표기 차이로 오매칭/누락 가능성이 있습니다.

### 선수 외부 링크(인스타/영상) 데이터
- `data/player_external_links.json`에 `player_id`별 링크를 수동으로 관리합니다. (URL + 출처 URL)
- UI에는 실링크가 있으면 버튼으로 노출하고, 없으면 검색 링크로 대체합니다.

## 사진 및 로스터 매핑
- `data/wkbl/roster_db.json`: 선수명-pno 매핑
- `data/wkbl/photo_overrides.json`: 사진 수동 오버라이드
- `data/wkbl/photo_blacklist.json`: 유효하지 않은 사진 제외

## 스케줄러 (launchd)
매일 04:10에 데이터를 자동 갱신합니다.
```bash
cp scripts/launchd/com.jeongsik.wkbl-refresh.plist ~/Library/LaunchAgents/
launchctl load ~/Library/LaunchAgents/com.jeongsik.wkbl-refresh.plist
```

## 개발 및 테스트
- **Python**: `pytest`
- **OCaml**: `dune runtest`

---
*WKBL 공식 사이트 AJAX 엔드포인트를 사용합니다.*
