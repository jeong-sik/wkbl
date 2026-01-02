"""
WKBL Moneyball Lab - 페이지 렌더링 테스트
모든 주요 페이지가 200 OK로 렌더링되는지 검증
"""
import pytest


class TestHomePage:
    """홈페이지 테스트"""

    def test_home_returns_200(self, client):
        """홈페이지 정상 렌더링"""
        response = client.get("/")
        assert response.status_code == 200
        assert "text/html" in response.headers["content-type"]

    def test_home_contains_title(self, client):
        """홈페이지에 타이틀 포함"""
        response = client.get("/")
        assert "WKBL" in response.text or "여자농구" in response.text


class TestPlayersPage:
    """선수 목록 페이지 테스트"""

    def test_players_returns_200(self, client):
        """선수 목록 페이지 정상 렌더링"""
        response = client.get("/players")
        assert response.status_code == 200

    def test_players_table_returns_200(self, client):
        """선수 테이블 HTMX partial 정상 렌더링"""
        response = client.get("/players/table")
        assert response.status_code == 200


class TestTeamsPage:
    """팀 목록 페이지 테스트"""

    def test_teams_returns_200(self, client):
        """팀 목록 페이지 정상 렌더링"""
        response = client.get("/teams")
        assert response.status_code == 200

    def test_teams_table_returns_200(self, client):
        """팀 테이블 HTMX partial 정상 렌더링"""
        response = client.get("/teams/table")
        assert response.status_code == 200


class TestStandingsPage:
    """순위표 페이지 테스트"""

    def test_standings_returns_200(self, client):
        """순위표 페이지 정상 렌더링"""
        response = client.get("/standings")
        assert response.status_code == 200

    def test_standings_table_returns_200(self, client):
        """순위표 테이블 HTMX partial 정상 렌더링"""
        response = client.get("/standings/table")
        assert response.status_code == 200


class TestGamesPage:
    """경기 목록 페이지 테스트"""

    def test_games_returns_200(self, client):
        """경기 목록 페이지 정상 렌더링"""
        response = client.get("/games")
        assert response.status_code == 200

    def test_games_table_returns_200(self, client):
        """경기 테이블 HTMX partial 정상 렌더링"""
        response = client.get("/games/table")
        assert response.status_code == 200


class TestBoxscoresPage:
    """박스스코어 목록 페이지 테스트"""

    def test_boxscores_returns_200(self, client):
        """박스스코어 목록 페이지 정상 렌더링"""
        response = client.get("/boxscores")
        assert response.status_code == 200

    def test_boxscores_table_returns_200(self, client):
        """박스스코어 테이블 HTMX partial 정상 렌더링"""
        response = client.get("/boxscores/table")
        assert response.status_code == 200


class TestComparePage:
    """비교 페이지 테스트 (레이더 차트 포함)"""

    def test_compare_returns_200(self, client):
        """비교 페이지 정상 렌더링"""
        response = client.get("/compare")
        assert response.status_code == 200

    def test_compare_table_returns_200(self, client):
        """비교 테이블 HTMX partial 정상 렌더링"""
        response = client.get("/compare/table")
        assert response.status_code == 200

    def test_compare_contains_chart_js(self, client):
        """비교 페이지에 Chart.js 포함 확인"""
        response = client.get("/compare")
        # base.html에서 Chart.js CDN 로드
        assert "chart.js" in response.text.lower() or "Chart" in response.text


class TestQAPage:
    """QA 페이지 테스트"""

    def test_qa_returns_200(self, client):
        """QA 페이지 정상 렌더링"""
        response = client.get("/qa")
        assert response.status_code == 200

    def test_qa_json_returns_200(self, client):
        """QA JSON API 정상 응답"""
        response = client.get("/qa.json")
        assert response.status_code == 200
        assert "application/json" in response.headers["content-type"]

    def test_qa_md_returns_200(self, client):
        """QA Markdown 정상 응답"""
        response = client.get("/qa.md")
        assert response.status_code == 200


class TestOCamlHealthCheck:
    """OCaml 서버 헬스체크 테스트"""

    def test_ocaml_health_endpoint_exists(self, client):
        """OCaml 헬스체크 엔드포인트 존재"""
        response = client.get("/ocaml/health")
        # OCaml 서버가 다운되어도 엔드포인트는 존재해야 함
        assert response.status_code in [200, 503]
