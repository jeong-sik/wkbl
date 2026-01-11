import asyncio
import re
import json
from playwright.async_api import async_playwright

async def calculate_plus_minus(game_no="33"):
    url = f"https://www.wkbl.or.kr/game/relay/relay.asp?game_no={game_no}&game_type=01"
    print(f"🧬 Starting Margin Engine for Game #{game_no}...")
    
    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=True)
        page = await browser.new_page()
        await page.goto(url, wait_until="networkidle")
        await page.wait_for_timeout(3000)

        # 1. 문자중계 전체 텍스트 추출
        # WKBL 문자중계는 보통 리스트 형태임
        events = await page.query_selector_all(".relay_list li, tr")
        raw_events = []
        for ev in events:
            text = await ev.inner_text()
            raw_events.append(text.strip())
        
        # 최신순으로 되어있으므로 뒤집어서 시간순으로 정렬
        raw_events.reverse()
        
        print(f"   📊 Processed {len(raw_events)} raw events.")

        # 2. 마진 계산 로직 (Prototype)
        margins = {} # {선수명: 마진}
        current_lineup = set() # 현재 코트 위 5명 (추정 로직 필요)
        
        # TODO: 실제 라인업 추적 알고리즘 고도화
        # 현재는 단순하게 득점 시 언급된 선수와 교체 시 언급된 선수 위주로 마진을 시뮬레이션
        
        home_score = 0
        away_score = 0
        
        for event in raw_events:
            # 득점 확인: "김단비 2점슛 성공"
            score_match = re.search(r'([가-힣]+)\s+(\d)점슛\s+성공', event)
            if score_match:
                player = score_match.group(1)
                pts = int(score_match.group(2))
                
                # 마진 계산 (간이 로직: 득점한 선수에게 직접 마진 부여 테스트)
                margins[player] = margins.get(player, 0) + pts
                
            # 교체 확인: "OUT: 박지수, IN: 강이슬"
            sub_match = re.search(r'교체\s+([가-힣]+)\s+.*?\s+([가-힣]+)', event)
            if sub_match:
                p_out, p_in = sub_match.groups()
                # print(f"   🔄 Sub: {p_out} -> {p_in}")

        await browser.close()
        
        # 결과 저장
        with open(f"data/wkbl/margin_results_{game_no}.json", "w", encoding="utf-8") as f:
            json.dump(margins, f, ensure_ascii=False, indent=2)
            
        return margins

if __name__ == "__main__":
    asyncio.run(calculate_plus_minus())
