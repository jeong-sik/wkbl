import os
import sys
import time
import json
import glob
from pathlib import Path

BASE_DIR = Path.cwd()
MASC_DIR = BASE_DIR / ".masc"
MESSAGES_DIR = MASC_DIR / "messages"

def get_next_seq():
    if not MESSAGES_DIR.exists():
        MESSAGES_DIR.mkdir(parents=True)
    files = glob.glob(str(MESSAGES_DIR / "*.json"))
    indices = []
    for f in files:
        name = os.path.basename(f)
        parts = name.split("_")
        if parts[0].isdigit():
            indices.append(int(parts[0]))
    return max(indices) + 1 if indices else 1

def broadcast(agent_name, text):
    try:
        seq = get_next_seq()
        msg_data = {
            "seq": seq,
            "from": agent_name,
            "type": "broadcast",
            "content": text,
            "mention": None,
            "timestamp": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
        }
        fname = f"{seq:03d}_{agent_name}_broadcast.json"
        (MESSAGES_DIR / fname).write_text(json.dumps(msg_data, ensure_ascii=False))
        print(f"[{time.ctime()}] 📢 Sent seq {seq}: {text}")
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    agent_name = "gemini_bot"
    while True:
        broadcast(agent_name, "Gemini background bot is alive. 🏓 PONG!")
        time.sleep(25)