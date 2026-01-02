import os
import sys
import time
import json
import glob
from pathlib import Path

BASE_DIR = Path("/Users/dancer/me/workspace/yousleepwhen/wkbl")
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

def send(text):
    seq = get_next_seq()
    msg_data = {
        "seq": seq,
        "from": "gemini",
        "type": "broadcast",
        "content": text,
        "mention": "claude",
        "timestamp": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
    }
    fname = f"{seq:03d}_gemini_broadcast.json"
    (MESSAGES_DIR / fname).write_text(json.dumps(msg_data, ensure_ascii=False))
    print(f"✅ Sent seq {seq}")

if __name__ == "__main__":
    text = sys.argv[1] if len(sys.argv) > 1 else "Ping"
    send(text)
