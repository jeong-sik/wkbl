"""
WKBL Moneyball Lab - pytest fixtures
Basketball-Reference 레벨 테스트 커버리지 목표
"""
import pytest
from fastapi.testclient import TestClient
import sys
from pathlib import Path

# app 모듈 import를 위한 경로 추가
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from app.main import app


@pytest.fixture
def client():
    """FastAPI TestClient fixture"""
    return TestClient(app)


@pytest.fixture
def sample_player_name():
    """테스트용 샘플 선수명"""
    return "박지수"


@pytest.fixture
def sample_team_code():
    """테스트용 샘플 팀 코드"""
    return "HAN"
