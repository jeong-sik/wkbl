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

# 로스터 및 사진 검증 포함
python3 scripts/wkbl_refresh_all.py --update-roster --audit-photos
```

### 개별 스크립트
- `scripts/wkbl_ajax_collector.py`: 공식 사이트 AJAX 데이터 수집
- `scripts/wkbl_aggregate_stats.py`: 수집된 CSV 집계
- `scripts/build_wkbl_db.py`: 집계된 CSV를 SQLite(`wkbl.db`)로 변환
- `scripts/wkbl_data_qa.py`: 데이터 무결성 체크

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