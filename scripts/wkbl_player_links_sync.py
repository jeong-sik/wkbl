#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import shutil
import sqlite3
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any


ROOT_DIR = Path(__file__).resolve().parents[1]
DEFAULT_DB_PATH = ROOT_DIR / "data" / "wkbl.db"
DEFAULT_LINKS_PATH = ROOT_DIR / "data" / "player_external_links.json"

ALLOWED_LINK_TYPES = {"instagram", "youtube"}


@dataclass(frozen=True)
class PlayerExternalLink:
    player_id: str
    link_type: str
    url: str
    source_url: str | None


def iso8601_utc() -> str:
    return datetime.now(timezone.utc).replace(microsecond=0).isoformat()


def ensure_schema(conn: sqlite3.Connection) -> None:
    conn.executescript(
        """
        CREATE TABLE IF NOT EXISTS player_external_links (
          player_id TEXT NOT NULL,
          link_type TEXT NOT NULL,
          url TEXT NOT NULL,
          source_url TEXT,
          scraped_at TEXT NOT NULL,
          PRIMARY KEY (player_id, link_type)
        );

        CREATE INDEX IF NOT EXISTS idx_player_external_links_player_id
          ON player_external_links(player_id);
        """
    )


def normalize_link_type(raw: str) -> str | None:
    s = str(raw or "").strip().lower()
    if not s:
        return None
    aliases = {
        "insta": "instagram",
        "ig": "instagram",
        "youtube": "youtube",
        "yt": "youtube",
    }
    s = aliases.get(s, s)
    if s in ALLOWED_LINK_TYPES:
        return s
    return None


def parse_player_id(raw: str) -> str | None:
    s = str(raw or "").strip()
    if not s or s.startswith("_"):
        return None
    if not s.isdigit():
        return None
    return s


def parse_link_entry(
    player_id: str,
    link_type_raw: str,
    payload: Any,
) -> PlayerExternalLink | None:
    link_type = normalize_link_type(link_type_raw)
    if link_type is None:
        return None

    if isinstance(payload, str):
        url = payload.strip()
        source_url: str | None = None
    elif isinstance(payload, dict):
        url = str(payload.get("url") or "").strip()
        source_url_raw = payload.get("source_url") or payload.get("source")
        source_url = str(source_url_raw).strip() if source_url_raw else None
    else:
        return None

    if not url:
        return None

    # Best-effort: accept http(s) only.
    if not (url.startswith("http://") or url.startswith("https://")):
        return None

    if source_url is not None and source_url != "" and not (
        source_url.startswith("http://") or source_url.startswith("https://")
    ):
        source_url = None

    return PlayerExternalLink(
        player_id=player_id,
        link_type=link_type,
        url=url,
        source_url=source_url,
    )


def load_links(path: Path) -> list[PlayerExternalLink]:
    raw = json.loads(path.read_text(encoding="utf-8"))
    links: list[PlayerExternalLink] = []

    if isinstance(raw, list):
        for item in raw:
            if not isinstance(item, dict):
                continue
            player_id = parse_player_id(item.get("player_id") or item.get("playerId"))
            if player_id is None:
                continue
            link_type_raw = item.get("link_type") or item.get("type")
            if not isinstance(link_type_raw, str):
                continue
            payload = {
                "url": item.get("url"),
                "source_url": item.get("source_url") or item.get("source"),
            }
            link = parse_link_entry(player_id, link_type_raw, payload)
            if link is not None:
                links.append(link)

    elif isinstance(raw, dict):
        for player_id_raw, player_payload in raw.items():
            player_id = parse_player_id(player_id_raw)
            if player_id is None:
                continue
            if not isinstance(player_payload, dict):
                continue
            for link_type_raw, payload in player_payload.items():
                link = parse_link_entry(player_id, str(link_type_raw), payload)
                if link is not None:
                    links.append(link)

    else:
        raise ValueError("Unsupported JSON format (expected object or array).")

    # De-dup by (player_id, link_type) keeping last entry.
    dedup: dict[tuple[str, str], PlayerExternalLink] = {}
    for link in links:
        dedup[(link.player_id, link.link_type)] = link
    return list(dedup.values())


def backup_db(db_path: Path) -> Path:
    ts = datetime.now().strftime("%Y%m%d-%H%M%S")
    backup_path = db_path.with_suffix(db_path.suffix + f".bak.{ts}")
    shutil.copy2(db_path, backup_path)
    return backup_path


def upsert_links(conn: sqlite3.Connection, links: list[PlayerExternalLink], *, scraped_at: str) -> int:
    cursor = conn.cursor()
    for link in links:
        cursor.execute(
            """
            INSERT INTO player_external_links(player_id, link_type, url, source_url, scraped_at)
            VALUES (?, ?, ?, ?, ?)
            ON CONFLICT(player_id, link_type) DO UPDATE SET
              url = excluded.url,
              source_url = excluded.source_url,
              scraped_at = excluded.scraped_at
            """,
            (link.player_id, link.link_type, link.url, link.source_url, scraped_at),
        )
    return len(links)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--db", default=str(DEFAULT_DB_PATH), help="SQLite DB path.")
    parser.add_argument(
        "--links-file",
        default=str(DEFAULT_LINKS_PATH),
        help="JSON file describing player external links.",
    )
    parser.add_argument("--backup", action="store_true", help="Backup DB before writing.")
    parser.add_argument("--dry-run", action="store_true", help="Parse and print summary only.")
    args = parser.parse_args()

    db_path = Path(args.db).expanduser()
    links_path = Path(args.links_file).expanduser()

    if not links_path.exists():
        raise SystemExit(f"Links file not found: {links_path}")

    links = load_links(links_path)
    links.sort(key=lambda x: (x.player_id, x.link_type))

    if not links:
        print("No external links found in file.")
        return

    counts: dict[str, int] = {}
    for link in links:
        counts[link.link_type] = counts.get(link.link_type, 0) + 1

    print(f"Loaded {len(links)} external links from {links_path}")
    print("By type:", ", ".join(f"{k}={v}" for k, v in sorted(counts.items())))

    if args.dry_run:
        for link in links[:10]:
            suffix = f" (source: {link.source_url})" if link.source_url else ""
            print(f"- {link.player_id} {link.link_type}: {link.url}{suffix}")
        if len(links) > 10:
            print(f"... ({len(links) - 10} more)")
        return

    db_path.parent.mkdir(parents=True, exist_ok=True)
    if args.backup and db_path.exists():
        backup_path = backup_db(db_path)
        print(f"DB backup written: {backup_path}")

    conn = sqlite3.connect(str(db_path))
    try:
        ensure_schema(conn)
        written = upsert_links(conn, links, scraped_at=iso8601_utc())
        conn.commit()
        print(f"Upserted {written} rows into player_external_links.")
    finally:
        conn.close()


if __name__ == "__main__":
    main()

