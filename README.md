# WKBL Moneyball Lab

WKBL 시즌 박스스코어를 수집해 basketball-reference 스타일로 탐색/비교하는 대시보드입니다.

## 구성
- `app/` FastAPI + Jinja2 UI
- `scripts/` 데이터 수집/집계 스크립트
- `data/wkbl/box/` 경기별 박스스코어 CSV
- `data/wkbl/derived/` 집계 결과 (players/teams/games)

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

## 실행
```bash
python3 app/main.py
# 또는
uvicorn app.main:app --reload
```

## 주요 페이지
- `/` 홈 (경기 단일 뷰)
- `/players` 선수 집계
- `/teams` 팀 집계
- `/games` 게임 로그
- `/compare` 선수 비교

## 메모
- WKBL 공식 사이트 AJAX 엔드포인트를 사용합니다.
