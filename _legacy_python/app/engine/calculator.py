import requests
import json

class WKBLMarginCalculator:
    def __init__(self, game_id):
        self.game_id = game_id
        self.home_score = 0
        self.away_score = 0
        self.players_on_court = {"home": set(), "away": set()}
        self.player_margins = {} # {name: margin}

    def fetch_data(self):
        url = f"https://api-gw.sports.naver.com/schedule/games/{self.game_id}/relay"
        headers = {"User-Agent": "Mozilla/5.0", "Referer": "https://sports.naver.com/"}
        res = requests.get(url, headers=headers)
        return res.json()

    def calculate(self):
        data = self.fetch_data()
        relays = data.get('result', {}).get('textRelayData', {}).get('textRelays', [])
        
        # 경기 종료부터 시작하므로 역순으로 정렬 (네이버는 최신순)
        relays.sort(key=lambda x: x.get('relayId', 0))

        for event in relays:
            text = event.get('text', '')
            home_pts = event.get('homeScore', 0)
            away_pts = event.get('awayScore', 0)
            
            # 득점 차이 발생 확인
            diff_home = home_pts - self.home_score
            diff_away = away_pts - self.away_score
            
            # 교체 기록 확인 (예: "박지수 OUT, 강이슬 IN")
            if "교체" in text or "IN" in text:
                # 여기서 코트 위 선수 업데이트 로직 (복잡하므로 정교한 파싱 필요)
                pass

            # 마진 업데이트 (코트 위 선수들에게 득점/실점 부여)
            # if diff_home > 0:
            #     for p in self.players_on_court["home"]: self.player_margins[p] += diff_home
            #     for p in self.players_on_court["away"]: self.player_margins[p] -= diff_home
            
            self.home_score = home_pts
            self.away_score = away_pts

        return self.player_margins

# 실전 투입 준비 중...
