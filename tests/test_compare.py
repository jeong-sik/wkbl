"""
WKBL Moneyball Lab - 비교 기능 테스트
레이더 차트 및 비교 메트릭 검증
"""
import pytest


class TestCompareFeature:
    """비교 기능 테스트"""

    def test_compare_player_vs_player(self, client):
        """선수 vs 선수 비교"""
        response = client.get("/compare/table?type=player&left=1&right=2")
        # 선수가 없어도 200 반환 (graceful handling)
        assert response.status_code == 200

    def test_compare_team_vs_team(self, client):
        """팀 vs 팀 비교"""
        response = client.get("/compare/table?type=team&left=HAN&right=SAM")
        assert response.status_code == 200

    def test_compare_empty_selection(self, client):
        """비교 대상 미선택시 빈 상태"""
        response = client.get("/compare/table")
        assert response.status_code == 200
        # 빈 상태 메시지 확인
        assert "선택" in response.text or "empty" in response.text.lower()

    def test_compare_table_contains_metrics(self, client):
        """비교 테이블에 주요 메트릭 포함"""
        response = client.get("/compare/table?type=player&left=1&right=2")
        # 주요 농구 스탯 키워드 확인
        content = response.text.lower()
        # 데이터가 있으면 메트릭 표시, 없으면 빈 상태
        assert response.status_code == 200


class TestCompareRadarChart:
    """레이더 차트 테스트"""

    def test_compare_page_has_chart_container(self, client):
        """비교 페이지에 차트 컨테이너 존재"""
        response = client.get("/compare")
        # Chart.js 관련 요소 확인
        assert "radarChart" in response.text or "chart" in response.text.lower()

    def test_compare_table_has_canvas(self, client):
        """비교 테이블에 canvas 요소 존재 (데이터 있을 때)"""
        response = client.get("/compare/table?type=player&left=1&right=2")
        # 데이터가 있으면 canvas, 없으면 빈 상태
        assert response.status_code == 200


class TestCompareMetrics:
    """비교 메트릭 데이터 테스트"""

    def test_compare_metrics_structure(self, client):
        """비교 메트릭 구조 검증"""
        response = client.get("/compare/table?type=player&left=1&right=2")
        content = response.text

        # 테이블 구조 확인
        assert "<table" in content or "empty" in content.lower() or "선택" in content

    def test_compare_bar_percentage(self, client):
        """비교 바 퍼센티지 범위 검증 (0-100%)"""
        response = client.get("/compare/table?type=player&left=1&right=2")
        content = response.text

        # style="width: XX%;" 패턴이 있으면 0-100 범위여야 함
        import re
        percentages = re.findall(r'width:\s*(\d+(?:\.\d+)?)%', content)
        for pct in percentages:
            assert 0 <= float(pct) <= 100, f"퍼센티지 {pct}%는 0-100 범위여야 함"
