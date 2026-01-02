# WKBL Moneyball Lab

WKBL 시즌 박스스코어를 수집해 basketball-reference 스타일로 탐색/비교하는 대시보드입니다.

## 구성
- `app/` FastAPI + Jinja2 UI (Python)
- `ocaml/` Dream + HTMX UI (OCaml) - 고성능 대체 엔진
- `scripts/` 데이터 수집/집계 스크립트
- `data/wkbl/box/` 경기별 박스스코어 CSV (로컬 생성, Git 미포함)
- `data/wkbl/derived/` 집계 결과 (players/teams/games, 로컬 생성, Git 미포함)
- `data/wkbl.db` SQLite 데이터베이스 (로컬 생성, Git 미포함)

## 설치
```bash
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

## 데이터 수집
```bash
python3 scripts/wkbl_ajax_collector.py
python3 scripts/wkbl_aggregate_stats.py
```
수집 데이터는 Git에 포함하지 않습니다. 로컬에서 생성/갱신하세요.

## 데이터 갱신 파이프라인
```bash
python3 scripts/wkbl_refresh_all.py
# collector 옵션 전달 (예: 10년치)
python3 scripts/wkbl_refresh_all.py -- --years 10
# 로스터 이미지 매핑까지 업데이트
python3 scripts/wkbl_refresh_all.py --update-roster --audit-photos -- --years 10
# 올스타/해외팀 경기까지 포함해서 집계
python3 scripts/wkbl_refresh_all.py --include-special -- --years 10
```
이 파이프라인은 `data/wkbl/*`를 로컬에 생성합니다.

## 데이터 QA
```bash
python3 scripts/wkbl_data_qa.py
```
보고서 경로: `data/wkbl/qa/qa_report.json`, `data/wkbl/qa/qa_report.md`

## SQLite DB (OCaml)
`data/wkbl/derived` CSV를 SQLite로 변환합니다.
```bash
python3 scripts/build_wkbl_db.py --force
```
생성된 DB는 `data/wkbl.db`이며 Git에 포함하지 않습니다.

## OCaml 엔진
Dream 프레임워크 기반의 고성능 대체 서버입니다. SQLite DB를 직접 읽어 HTMX UI를 제공합니다.

### 빌드
```bash
cd ocaml
opam install . --deps-only  # 의존성 설치
dune build                   # 빌드
```

### 실행
```bash
cd ocaml
dune exec wkbl              # 기본 실행 (포트 8080)
# 또는
WKBL_DB=../data/wkbl.db dune exec wkbl
```

### 엔드포인트
- `/` 홈 (선수 목록)
- `/players` 선수 집계 (검색/정렬)
- `/teams` 팀 집계 (시즌/범위 필터)
- `/qa` 데이터 QA 대시보드
- `/health` 헬스체크

## 로스터 이미지 매핑
```bash
python3 scripts/wkbl_roster_live.py
```
WKBL 현역/은퇴/외국인 선수 목록에서 pno를 수집해 `data/wkbl/roster_db.json`을 갱신합니다.

## 사진 검증
```bash
python3 scripts/wkbl_photo_audit.py
```
누락된 WKBL 이미지(pno)를 `data/wkbl/photo_blacklist.json`에 기록합니다.

## 사진 수동 매핑
`data/wkbl/photo_overrides.json`에서 특정 선수의 사진을 직접 지정할 수 있습니다.
```json
{
  "photo_by_name_team": {
    "홍길동|우리은행": "https://example.com/player.png",
    "Jane Doe|ENEOS": "095999"
  },
  "photo_by_name": {
    "홍길동": "https://example.com/player.png"
  }
}
```
값이 숫자면 pno로 처리하고, URL이면 그대로 사용합니다.

## 스케줄러 (launchd)
```bash
cp scripts/launchd/com.jeongsik.wkbl-refresh.plist ~/Library/LaunchAgents/
launchctl load ~/Library/LaunchAgents/com.jeongsik.wkbl-refresh.plist
```
기본 실행 시간: 매일 04:10. 로그는 `~/me/logs/wkbl_refresh.log`와 `~/me/logs/wkbl_refresh.launchd.log`에 저장됩니다.

### 10년치 백필 (옵션)
```bash
python3 scripts/wkbl_ajax_collector.py --years 10
python3 scripts/wkbl_aggregate_stats.py
```

## 실행
```bash
python3 app/main.py
# 또는
uvicorn app.main:app --reload
```

## 주요 페이지 (Python 서버)
- `/` 홈 (경기 단일 뷰)
- `/players` 선수 집계
- `/teams` 팀 집계
- `/standings` 시즌 순위표
- `/games` 게임 로그
- `/boxscores` 박스스코어 목록
- `/boxscore?game_key=...` 박스스코어 상세
- `/compare` 선수 비교
- `/leaders` 리더보드 (스탯 비교 메트릭)
- `/qa` 데이터 QA 대시보드
- `/qa.json` QA JSON
- `/qa.md` QA Markdown

### OCaml 프록시 엔드포인트
Python 서버에서 OCaml 엔진으로 요청을 프록시합니다:
- `/ocaml/health` OCaml 헬스체크
- `/ocaml/predict` 예측 API (준비중)

## 메모
- WKBL 공식 사이트 AJAX 엔드포인트를 사용합니다.
