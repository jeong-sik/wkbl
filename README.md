# WKBL Moneyball Lab

WKBL 시즌 박스스코어를 수집해 basketball-reference 스타일로 탐색/비교하는 대시보드입니다. **Kirin (OCaml 5.x + Eio)** 엔진이 웹 서버와 데이터 파이프라인을 모두 담당합니다.

## 아키텍처
- **Engine**: `ocaml/` (Kirin Framework) - 고성능 통계 엔진, UI 렌더링, 데이터 수집기(Scraper)
- **Database**: PostgreSQL (Supabase) - `scraper_tool`이 직접 데이터 동기화
- **Deployment**: Docker (Distroless) - 순수 OCaml 바이너리 배포

## 빠른 시작 (로컬 개발)

### 1. 환경 설정
PostgreSQL 연결 URL이 필요합니다. (Supabase 권장)
```bash
export DATABASE_URL="postgresql://postgres:password@db.supabase.co:5432/postgres"
```

### 2. 데이터 동기화 (Kirin Scraper)
Python 스크립트 없이 OCaml 툴로 직접 데이터를 수집하고 DB에 넣습니다.
```bash
cd ocaml

# 빌드
dune build

# 1. 스케줄 동기화 (현재 시즌)
dune exec bin/scraper_tool.exe sync schedule

# 2. 전체 역사 동기화 (1998~현재, 약 43개 시즌) - *시간 소요*
dune exec bin/scraper_tool.exe sync history
```

### 3. 웹 서버 실행
```bash
dune exec bin/main.exe  # 포트 8000에서 실행
```

## 배포 (Docker)
이 레포는 루트의 `Dockerfile`로 경량화된 컨테이너(Distroless)를 배포합니다. DB는 포함되지 않으며 외부 PostgreSQL에 연결해야 합니다.

### 로컬 실행
```bash
docker build -t wkbl .
docker run --rm -p 8000:8080 -e DATABASE_URL="postgresql://..." wkbl
```

### Railway 배포
1. Railway 프로젝트 생성 및 GitHub 연결.
2. `DATABASE_URL` 환경변수 설정.
3. 배포 시 `scraper_tool`도 함께 빌드되므로, 필요 시 Railway CLI나 Cron Job으로 데이터 갱신 가능.

## CLI 도구 (`scraper_tool`)
데이터 수집 및 관리를 위한 강력한 CLI 도구가 내장되어 있습니다.

```bash
# 도움말
dune exec bin/scraper_tool.exe -- --help

# 데이터 조회 (CSV 출력 가능)
dune exec bin/scraper_tool.exe draft --csv          # 드래프트 전체
dune exec bin/scraper_tool.exe games --season=046   # 특정 시즌 경기 결과
dune exec bin/scraper_tool.exe salary --csv         # 연봉 현황

# 데이터 동기화 (DB 적재)
dune exec bin/scraper_tool.exe sync schedule        # 최신 스케줄/결과 동기화
dune exec bin/scraper_tool.exe sync history         # 전체 역사 동기화
```

## 스케줄러 (Automation)
매일 새벽 데이터를 갱신하려면 `scraper_tool sync schedule`을 주기적으로 실행하면 됩니다.
(Railway Cron Job 또는 GitHub Actions Schedule 사용 권장)

## 개발 및 테스트
- **Test**: `dune runtest`
- **Lint**: `dune build @fmt`

---
*Powered by OCaml 5.x & Eio*
