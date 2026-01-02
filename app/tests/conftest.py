import pytest
import subprocess
import time
import os
from pathlib import Path

# 프로젝트 루트 찾기
ROOT_DIR = Path(__file__).resolve().parent.parent.parent
SCRIPT_PATH = ROOT_DIR / "scripts" / "manage_ocaml.sh"

@pytest.fixture(scope="session", autouse=True)
def ocaml_server():
    """
    테스트 세션 시작 전 OCaml 서버를 띄우고, 종료 후 내립니다.
    OCaml 연동 테스트를 위해 필수적입니다.
    """
    if not SCRIPT_PATH.exists():
        print(f"\n[Warning] OCaml script not found at {SCRIPT_PATH}")
        yield
        return

    print("\n[Fixture] Starting OCaml server for tests...")
    # 스크립트 실행 권한 확인
    if not os.access(SCRIPT_PATH, os.X_OK):
        os.chmod(SCRIPT_PATH, 0o755)

    subprocess.run([str(SCRIPT_PATH), "start"], check=False)
    time.sleep(2)  # Wait for startup
    
    yield
    
    print("\n[Fixture] Stopping OCaml server...")
    subprocess.run([str(SCRIPT_PATH), "stop"], check=False)

