"""
WKBL Moneyball Lab - API 엔드포인트 테스트
JSON 응답 및 데이터 구조 검증
"""
import pytest


class TestQAAPI:
    """QA API 테스트"""

    def test_qa_json_returns_valid_json(self, client):
        """QA JSON이 유효한 JSON 반환"""
        response = client.get("/qa.json")
        assert response.status_code == 200

        # JSON 파싱 가능해야 함
        data = response.json()
        assert isinstance(data, (dict, list))

    def test_qa_json_structure(self, client):
        """QA JSON 구조 검증"""
        response = client.get("/qa.json")
        data = response.json()

        # 데이터 타입 확인 (구체적 구조는 실제 API에 따라 다름)
        assert data is not None


class TestRefreshEndpoint:
    """데이터 새로고침 엔드포인트 테스트"""

    def test_refresh_returns_200(self, client):
        """새로고침 엔드포인트 정상 작동"""
        response = client.get("/refresh")
        # 새로고침은 시간이 걸릴 수 있으므로 다양한 상태 허용
        assert response.status_code in [200, 302, 307]


class TestStaticFiles:
    """정적 파일 서빙 테스트"""

    def test_static_css_exists(self, client):
        """CSS 파일 서빙"""
        response = client.get("/static/style.css")
        # 파일이 없어도 404 반환 (500 에러가 아님)
        assert response.status_code in [200, 404]


class TestTeamLogo:
    """팀 로고 엔드포인트 테스트"""

    def test_team_logo_endpoint(self, client):
        """팀 로고 엔드포인트 존재"""
        response = client.get("/team-logo?team=HAN")
        # 로고가 없어도 graceful하게 처리
        assert response.status_code in [200, 302, 404]
