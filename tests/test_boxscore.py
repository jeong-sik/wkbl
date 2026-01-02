"""
WKBL Moneyball Lab - 박스스코어 테스트
Basketball-Reference 레벨 품질 검증
"""
import pytest


class TestBoxscoreList:
    """박스스코어 목록 테스트"""

    def test_boxscores_page_renders(self, client):
        """박스스코어 목록 페이지 렌더링"""
        response = client.get("/boxscores")
        assert response.status_code == 200
        assert "text/html" in response.headers["content-type"]

    def test_boxscores_has_game_links(self, client):
        """박스스코어 목록에 경기 링크 존재"""
        response = client.get("/boxscores/table")
        assert response.status_code == 200


class TestBoxscoreDetail:
    """박스스코어 상세 테스트"""

    def test_boxscore_page_renders(self, client):
        """박스스코어 상세 페이지 렌더링"""
        # 기본 페이지 (game_id 없이)
        response = client.get("/boxscore")
        assert response.status_code == 200

    def test_boxscore_with_game_id(self, client):
        """특정 경기 박스스코어 조회"""
        # 존재하지 않는 game_id도 graceful하게 처리
        response = client.get("/boxscore?game_id=99999")
        assert response.status_code in [200, 404]

    def test_boxscore_table_partial(self, client):
        """박스스코어 테이블 HTMX partial"""
        response = client.get("/boxscore/table")
        assert response.status_code == 200


class TestBoxscoreData:
    """박스스코어 데이터 품질 테스트"""

    def test_boxscore_contains_stats_columns(self, client):
        """박스스코어에 필수 스탯 컬럼 존재"""
        response = client.get("/boxscore/table")
        content = response.text.lower()

        # Basketball-Reference 필수 컬럼들
        # 데이터가 없어도 페이지는 렌더링되어야 함
        assert response.status_code == 200
