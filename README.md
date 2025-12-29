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

## 데이터 갱신 파이프라인
```bash
python3 scripts/wkbl_refresh_all.py
# collector 옵션 전달 (예: 10년치)
python3 scripts/wkbl_refresh_all.py -- --years 10
```

## 데이터 QA
```bash
python3 scripts/wkbl_data_qa.py
```
보고서 경로: `data/wkbl/qa/qa_report.json`, `data/wkbl/qa/qa_report.md`

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

## 주요 페이지
- `/` 홈 (경기 단일 뷰)
- `/players` 선수 집계
- `/teams` 팀 집계
- `/standings` 시즌 순위표
- `/games` 게임 로그
- `/boxscores` 박스스코어 목록
- `/boxscore?game_key=...` 박스스코어 상세
- `/compare` 선수 비교
- `/qa` 데이터 QA 대시보드
- `/qa.json` QA JSON
- `/qa.md` QA Markdown

## 메모
- WKBL 공식 사이트 AJAX 엔드포인트를 사용합니다.
