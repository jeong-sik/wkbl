# VPS 배포 (월 ~$5) + Cloudflare Tunnel

OCaml(Dream) 서버를 **VPS에 상시 실행**하고, Cloudflare Tunnel로 `wkbl.crying.pictures` 같은 도메인을 붙이는 “가장 싸고 덜 귀찮은” 배포 루트입니다.

- 서버: Dream(OCaml) + SQLite(`wkbl.db`)
- 배포: Docker Compose
- 외부 노출: Cloudflare Tunnel (무료)
- 트래픽 분석: Cloudflare Web Analytics (무료, 옵션)

## 0) 준비물

- Ubuntu 22.04/24.04 같은 리눅스 VPS 1대 (보통 $5~6/월)
- Cloudflare 도메인 + Zero Trust 사용 가능 상태

## 1) VPS에서 Docker 설치

```bash
sudo apt-get update
sudo apt-get install -y ca-certificates curl git
curl -fsSL https://get.docker.com | sudo sh
sudo usermod -aG docker $USER
newgrp docker
docker version
docker compose version
```

## 2) 레포 클론

```bash
git clone https://github.com/jeong-sik/wkbl.git
cd wkbl
```

## 3) Cloudflare Tunnel 생성 (Zero Trust)

Zero Trust Dashboard에서:

1. `Networks` → `Tunnels` → `Create a tunnel`
2. Connector는 `Docker` 선택 후 **token** 복사
3. `Public Hostnames` 추가:
   - `wkbl.crying.pictures` → `http://wkbl:8000`
   - (docker-compose에서 컨테이너 이름이 `wkbl`입니다)

## 4) 실행 (docker-compose)

```bash
cd deploy
cp .env.example .env
# .env에서 CLOUDFLARE_TUNNEL_TOKEN 채우기
docker compose up -d --build
docker compose ps
docker compose logs -f
```

VPS 내부에서 확인:

```bash
curl -fsSL http://127.0.0.1:8000/health
```

브라우저에서:

- `https://wkbl.crying.pictures/health`

## 5) 업데이트/데이터 갱신

코드 업데이트 + 재빌드:

```bash
cd wkbl
git pull
cd deploy
docker compose up -d --build
```

데이터 갱신(재수집/재집계 포함)은 `Dockerfile` 빌드 단계에서 일어나므로, 캐시를 깨고 빌드하면 됩니다:

```bash
cd wkbl/deploy
docker compose build --no-cache wkbl
docker compose up -d --force-recreate wkbl
```

## 6) (옵션) 무료 트래픽 분석: Cloudflare Web Analytics

Cloudflare Web Analytics에서 token을 만든 뒤, `deploy/.env`에 넣으면 됩니다:

- `CF_WEB_ANALYTICS_TOKEN=<token>`

설정 시 HTML에 자동으로 beacon 스크립트가 삽입됩니다.

## 트러블슈팅

- 빌드가 느리거나 메모리 부족(OOM) 나면: VPS에 swap 추가 후 다시 빌드.
- 터널이 연결되는데 사이트가 안 뜨면: Zero Trust의 `Public Hostname`이 `http://wkbl:8000`로 되어 있는지, `docker compose ps`에서 두 컨테이너가 `Up`인지 확인.

