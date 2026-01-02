import requests
from bs4 import BeautifulSoup
import json

URL = "https://www.wkbl.or.kr/game/sch/schedule1.asp"
HEADERS = {"User-Agent": "Mozilla/5.0"}

def get_all_seasons():
    try:
        response = requests.get(URL, headers=HEADERS)
        response.raise_for_status()
        response.encoding = 'euc-kr'
        
        soup = BeautifulSoup(response.text, "html.parser")
        select = soup.select_one("select#season_gu")
        
        seasons = []
        if select:
            for option in select.find_all("option"):
                code = option['value']
                name = option.text.strip()
                if code:
                    seasons.append({"code": code, "name": name})
        
        return seasons
    except Exception as e:
        print(f"Error: {e}")
        return []

if __name__ == "__main__":
    seasons = get_all_seasons()
    print(json.dumps(seasons, indent=4, ensure_ascii=False))
    
    with open("wkbl_seasons.json", "w", encoding="utf-8") as f:
        json.dump(seasons, f, indent=4, ensure_ascii=False)
