#!/usr/bin/env python3
from __future__ import annotations

import argparse
import html
import re
import shutil
import sqlite3
import time
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path

import requests
from bs4 import BeautifulSoup


ROOT_DIR = Path(__file__).resolve().parents[1]
DEFAULT_DB_PATH = ROOT_DIR / "data" / "wkbl.db"

PLAYER_DETAIL_URL = "https://www.wkbl.or.kr/player/detail2.asp"
TRADE_AJAX_URL = "https://www.wkbl.or.kr/player/ajax/ajax_trade_info.asp"

REQUEST_TIMEOUT_SEC = 15
REQUEST_DELAY_SEC = 0.7
HEADERS = {
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language": "ko-KR,ko;q=0.9,en-US;q=0.8,en;q=0.7",
}

PLAYER_GROUPS = ("12", "11", "F11")
TRADE_RANGES = (
    (2021, 2030),
    (2011, 2020),
    (2001, 2010),
    (1999, 2000),
)


@dataclass(frozen=True)
class PlayerDraft:
    player_id: str
    raw_text: str
    draft_year: int | None
    draft_round: int | None
    pick_in_round: int | None
    overall_pick: int | None
    draft_team: str | None
    source_url: str
    scraped_at: str


@dataclass(frozen=True)
class TradeEvent:
    event_date: str  # YYYY-MM-DD
    event_year: int
    event_text: str
    event_text_norm: str
    source_url: str
    scraped_at: str


def iso8601_utc() -> str:
    return datetime.now(timezone.utc).replace(microsecond=0).isoformat()


def normalize_text(s: str) -> str:
    text = str(s or "")
    buf: list[str] = []
    prev_space = True

    def add_space() -> None:
        nonlocal prev_space
        if not prev_space:
            buf.append(" ")
            prev_space = True

    for ch in text:
        if ch in {'"', "\\"}:
            add_space()
            continue

        code = ord(ch)
        is_space = (
            ch in {" ", "\t", "\n", "\r"}
            or code == 0x00A0  # NBSP
            or code == 0x3000  # IDEOGRAPHIC SPACE
            or code == 0xFEFF  # BOM
            or code in {0x200B, 0x200C, 0x200D, 0x2009, 0x202F}  # zero-width/thin spaces
        )
        if is_space:
            add_space()
            continue

        buf.append(ch)
        prev_space = False

    return "".join(buf).strip()


def ensure_schema(conn: sqlite3.Connection) -> None:
    conn.executescript(
        """
        CREATE TABLE IF NOT EXISTS player_drafts (
          player_id TEXT PRIMARY KEY,
          draft_year INTEGER,
          draft_round INTEGER,
          pick_in_round INTEGER,
          overall_pick INTEGER,
          draft_team TEXT,
          raw_text TEXT NOT NULL,
          source_url TEXT NOT NULL,
          scraped_at TEXT NOT NULL
        );

        CREATE INDEX IF NOT EXISTS idx_player_drafts_year ON player_drafts(draft_year);

        CREATE TABLE IF NOT EXISTS official_trade_events (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          event_date TEXT NOT NULL,
          event_year INTEGER NOT NULL,
          event_text TEXT NOT NULL,
          event_text_norm TEXT NOT NULL,
          source_url TEXT NOT NULL,
          scraped_at TEXT NOT NULL,
          UNIQUE(event_date, event_text)
        );

        CREATE INDEX IF NOT EXISTS idx_trade_events_date ON official_trade_events(event_date);
        CREATE INDEX IF NOT EXISTS idx_trade_events_year ON official_trade_events(event_year);
        """
    )


def fetch_player_detail_html(player_id: str) -> tuple[str | None, str | None]:
    for group in PLAYER_GROUPS:
        params = {"player_group": group, "tcode": "", "pno": player_id}
        try:
            resp = requests.get(
                PLAYER_DETAIL_URL,
                params=params,
                headers=HEADERS,
                timeout=REQUEST_TIMEOUT_SEC,
            )
            resp.raise_for_status()
            resp.encoding = "utf-8"
            text = resp.text
        except Exception:
            time.sleep(REQUEST_DELAY_SEC)
            continue

        if "player_info" in text and player_id in text:
            url = resp.url
            return text, url
        time.sleep(REQUEST_DELAY_SEC)

    return None, None


def parse_player_draft(player_id: str, html_text: str, *, source_url: str) -> PlayerDraft | None:
    soup = BeautifulSoup(html_text, "html.parser")
    draft_li = soup.select_one(".player_draft .list_text li")
    if draft_li is None:
        return None

    raw = draft_li.get_text(" ", strip=True)
    raw = normalize_text(raw)
    if not raw or raw in {"-", "없음"}:
        return None

    # Drop the leading label "드래프트" if present.
    raw_clean = raw
    if raw_clean.startswith("드래프트"):
        raw_clean = normalize_text(raw_clean[len("드래프트") :])

    # Example:
    # 2017 WKBL 신입선수선발회 / 1라운드 / 1순위 (전체 1순위) / KB스타즈
    m = re.search(
        r"(?P<year>\d{4})\s*WKBL.*?/\s*(?P<round>\d+)\s*라운드\s*/\s*(?P<pick>\d+)\s*순위"
        r"(?:\s*\(전체\s*(?P<overall>\d+)\s*순위\))?\s*/\s*(?P<team>.+)$",
        raw_clean,
    )
    if m is None:
        return PlayerDraft(
            player_id=player_id,
            raw_text=raw_clean,
            draft_year=None,
            draft_round=None,
            pick_in_round=None,
            overall_pick=None,
            draft_team=None,
            source_url=source_url,
            scraped_at=iso8601_utc(),
        )

    def to_int(s: str | None) -> int | None:
        if not s:
            return None
        try:
            return int(s)
        except ValueError:
            return None

    return PlayerDraft(
        player_id=player_id,
        raw_text=raw_clean,
        draft_year=to_int(m.group("year")),
        draft_round=to_int(m.group("round")),
        pick_in_round=to_int(m.group("pick")),
        overall_pick=to_int(m.group("overall")),
        draft_team=normalize_text(m.group("team")),
        source_url=source_url,
        scraped_at=iso8601_utc(),
    )


def load_player_ids(conn: sqlite3.Connection, *, limit: int, only_missing: bool) -> list[str]:
    cursor = conn.cursor()
    if only_missing:
        cursor.execute(
            """
            SELECT p.player_id
            FROM players p
            LEFT JOIN player_drafts d ON d.player_id = p.player_id
            WHERE d.player_id IS NULL
            ORDER BY p.player_id
            """
        )
    else:
        cursor.execute("SELECT player_id FROM players ORDER BY player_id")
    rows = [str(pid) for (pid,) in cursor.fetchall()]
    if limit and limit > 0:
        return rows[:limit]
    return rows


def upsert_player_draft(conn: sqlite3.Connection, draft: PlayerDraft) -> None:
    conn.execute(
        """
        INSERT OR REPLACE INTO player_drafts(
          player_id, draft_year, draft_round, pick_in_round, overall_pick, draft_team,
          raw_text, source_url, scraped_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        """,
        (
            draft.player_id,
            draft.draft_year,
            draft.draft_round,
            draft.pick_in_round,
            draft.overall_pick,
            draft.draft_team,
            draft.raw_text,
            draft.source_url,
            draft.scraped_at,
        ),
    )


def fetch_trade_events_html(*, start_year: int, end_year: int) -> str:
    resp = requests.post(
        TRADE_AJAX_URL,
        data={"sDate": start_year, "eDate": end_year},
        headers=HEADERS,
        timeout=REQUEST_TIMEOUT_SEC,
    )
    resp.raise_for_status()
    resp.encoding = "utf-8"
    return resp.text


def parse_trade_events(html_text: str, *, source_url: str) -> list[TradeEvent]:
    soup = BeautifulSoup(html_text, "html.parser")
    out: list[TradeEvent] = []
    scraped_at = iso8601_utc()

    for block in soup.select("div.wrap_trade"):
        year_str = block.select_one("strong.point_year")
        if year_str is None:
            continue
        try:
            year = int(normalize_text(year_str.get_text(strip=True)))
        except ValueError:
            continue

        for li in block.select("ul.list_history > li"):
            date_el = li.select_one("span.txt_date")
            text_el = li.select_one("span.txt_history")
            if date_el is None or text_el is None:
                continue

            date_text = normalize_text(date_el.get_text(strip=True))
            m = re.search(r"(\d{1,2})\s*월\s*(\d{1,2})\s*일", date_text)
            if m is None:
                continue
            month = int(m.group(1))
            day = int(m.group(2))
            event_date = f"{year:04d}-{month:02d}-{day:02d}"

            event_text = text_el.get_text(" ", strip=True)
            event_text = html.unescape(event_text)
            event_text = normalize_text(event_text)
            if not event_text:
                continue

            out.append(
                TradeEvent(
                    event_date=event_date,
                    event_year=year,
                    event_text=event_text,
                    event_text_norm=normalize_text(event_text),
                    source_url=source_url,
                    scraped_at=scraped_at,
                )
            )

    return out


def upsert_trade_events(conn: sqlite3.Connection, events: list[TradeEvent]) -> int:
    if not events:
        return 0

    conn.executemany(
        """
        INSERT OR REPLACE INTO official_trade_events(
          event_date, event_year, event_text, event_text_norm, source_url, scraped_at
        ) VALUES (?, ?, ?, ?, ?, ?)
        """,
        [
            (
                e.event_date,
                e.event_year,
                e.event_text,
                e.event_text_norm,
                e.source_url,
                e.scraped_at,
            )
            for e in events
        ],
    )
    return len(events)


def main() -> int:
    parser = argparse.ArgumentParser(description="Sync official Draft/Trade data from wkbl.or.kr into SQLite.")
    parser.add_argument("--db", type=Path, default=DEFAULT_DB_PATH)
    parser.add_argument("--backup", action="store_true", help="Create a .bak copy next to the DB before edits.")
    parser.add_argument("--dry-run", action="store_true", help="Fetch and report without writing DB.")
    parser.add_argument("--limit", type=int, default=0, help="Limit players processed for draft sync (0=all).")
    parser.add_argument("--only-missing", action="store_true", help="Only fetch players missing draft rows.")
    parser.add_argument("--skip-draft", action="store_true", help="Skip per-player draft sync.")
    parser.add_argument("--skip-trade", action="store_true", help="Skip official trade/FA events sync.")
    parser.add_argument("--delay", type=float, default=REQUEST_DELAY_SEC, help="Delay seconds between requests.")
    args = parser.parse_args()

    if not args.db.exists():
        raise SystemExit(f"DB not found: {args.db}")

    if args.backup and not args.dry_run:
        backup_path = args.db.with_suffix(args.db.suffix + ".bak")
        shutil.copy2(args.db, backup_path)
        print(f"Backup created: {backup_path}")

    conn = sqlite3.connect(args.db)
    try:
        ensure_schema(conn)

        if not args.skip_draft:
            player_ids = load_player_ids(conn, limit=args.limit, only_missing=args.only_missing)
            if not player_ids:
                print("No players found.")
            else:
                ok = 0
                missing = 0
                for i, pid in enumerate(player_ids, start=1):
                    print(f"[draft {i}/{len(player_ids)}] {pid} ...", end=" ")
                    html_text, source_url = fetch_player_detail_html(pid)
                    if html_text is None or source_url is None:
                        missing += 1
                        print("miss")
                        time.sleep(args.delay)
                        continue

                    draft = parse_player_draft(pid, html_text, source_url=source_url)
                    if draft is None:
                        missing += 1
                        print("none")
                        time.sleep(args.delay)
                        continue

                    ok += 1
                    if not args.dry_run:
                        upsert_player_draft(conn, draft)
                        conn.commit()
                    print(draft.raw_text)
                    time.sleep(args.delay)

                print(f"Draft sync: ok={ok} missing={missing}")

        if not args.skip_trade:
            total = 0
            for start_year, end_year in TRADE_RANGES:
                print(f"[trade] {start_year}~{end_year} ...", end=" ")
                try:
                    html_text = fetch_trade_events_html(start_year=start_year, end_year=end_year)
                except Exception as exc:  # noqa: BLE001
                    print(f"error={exc}")
                    time.sleep(args.delay)
                    continue

                events = parse_trade_events(html_text, source_url=TRADE_AJAX_URL)
                if args.dry_run:
                    total += len(events)
                    print(f"events={len(events)} (dry-run)")
                else:
                    inserted = upsert_trade_events(conn, events)
                    conn.commit()
                    total += inserted
                    print(f"events={inserted}")
                time.sleep(args.delay)

            print(f"Trade sync total events={total}")

        return 0
    finally:
        conn.close()


if __name__ == "__main__":
    raise SystemExit(main())

