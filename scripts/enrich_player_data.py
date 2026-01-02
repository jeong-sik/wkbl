#!/usr/bin/env python3
"""
Player Data Enrichment Script
Extracts position data from crawled JSON/HTML files and updates wkbl.db

Data Sources:
- data/raw_crawled/advanced_stats.json
- data/raw_crawled/parsed_boxscore.json
- data/raw_crawled/{season}/*_player.html

Created by: data-enricher agent (MASC)
"""

import json
import re
import sqlite3
from pathlib import Path


def load_json_positions(data_dir: Path) -> dict[str, str]:
    """Extract player positions from JSON files."""
    positions = {}

    json_files = [
        data_dir / "advanced_stats.json",
        data_dir / "parsed_boxscore.json",
    ]

    for json_file in json_files:
        if not json_file.exists():
            print(f"  Skipping {json_file.name} (not found)")
            continue

        with open(json_file, "r", encoding="utf-8") as f:
            data = json.load(f)

        for team_name, players in data.items():
            for p in players:
                pid = p.get("player_id")
                pos = p.get("position")
                if pid and pos and pos in ("G", "F", "C"):
                    positions[pid] = pos

    print(f"  JSON files: {len(positions)} positions")
    return positions


def parse_html_positions(data_dir: Path) -> dict[str, str]:
    """Extract player positions from HTML boxscore files."""
    positions = {}

    # Pattern to extract player ID and position from HTML
    # Example: title="095450"> ... <span class="position">G</span>
    player_pattern = re.compile(
        r'title="(\d{6})"[^>]*>.*?<span class="position">([GFC])</span>',
        re.DOTALL
    )

    # Alternative pattern from table rows
    # <td ... data-player="095450" ... >...</td> with position in same row
    table_pattern = re.compile(
        r'data-player="(\d{6})".*?<td[^>]*class="[^"]*position[^"]*"[^>]*>([GFC])</td>',
        re.DOTALL
    )

    # Pattern for boxscore tables
    boxscore_pattern = re.compile(
        r'<tr[^>]*>.*?<td[^>]*>(\d{6})</td>.*?<td[^>]*>([GFC])</td>',
        re.DOTALL
    )

    # Scan all season directories
    for season_dir in data_dir.iterdir():
        if not season_dir.is_dir() or season_dir.name == "raw":
            continue

        html_files = list(season_dir.glob("*_player.html"))
        for html_file in html_files:
            try:
                content = html_file.read_text(encoding="utf-8", errors="ignore")

                # Try primary pattern
                for match in player_pattern.finditer(content):
                    pid, pos = match.groups()
                    if pid and pos:
                        positions[pid] = pos

                # Try table pattern
                for match in table_pattern.finditer(content):
                    pid, pos = match.groups()
                    if pid and pos:
                        positions[pid] = pos

            except Exception as e:
                print(f"  Error parsing {html_file.name}: {e}")

    print(f"  HTML files: {len(positions)} positions")
    return positions


def parse_detailed_html(data_dir: Path) -> dict[str, str]:
    """Parse HTML with more detailed regex patterns."""
    positions = {}

    # Pattern: Looking for player ID in links and position in spans
    patterns = [
        # Pattern 1: title attribute with player ID, position in nearby span
        re.compile(r'title="(\d{6})"[^>]*>[\s\S]{0,500}<span class="position">([GFC])</span>'),
        # Pattern 2: onclick with player ID
        re.compile(r'onClick="fnPlayerInfo\([^)]*\'(\d{6})\'[^)]*\)"[\s\S]{0,300}<span class="position">([GFC])</span>'),
        # Pattern 3: data-player attribute
        re.compile(r'data-player="(\d{6})"[\s\S]{0,300}<span class="position">([GFC])</span>'),
    ]

    for season_dir in data_dir.iterdir():
        if not season_dir.is_dir() or season_dir.name == "raw":
            continue

        for html_file in season_dir.glob("*_player.html"):
            try:
                content = html_file.read_text(encoding="utf-8", errors="ignore")

                # Also try: look for all position spans and nearby player IDs
                pos_spans = re.finditer(r'<span class="position">([GFC])</span>', content)
                for pos_match in pos_spans:
                    pos = pos_match.group(1)
                    # Look backwards for player ID
                    start = max(0, pos_match.start() - 1000)
                    context = content[start:pos_match.start()]

                    # Find closest player ID
                    id_matches = list(re.finditer(r'(?:title="|data-player="|/m_)(\d{6})', context))
                    if id_matches:
                        pid = id_matches[-1].group(1)  # Take the closest one
                        positions[pid] = pos

            except Exception as e:
                continue

    return positions


def update_database(db_path: Path, positions: dict[str, str], dry_run: bool = False) -> tuple[int, int]:
    """Update player positions in database."""
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    # Get current players without position
    cursor.execute("SELECT player_id, position FROM players")
    all_players = cursor.fetchall()

    updated = 0
    skipped = 0

    for player_id, current_pos in all_players:
        if player_id in positions:
            new_pos = positions[player_id]
            if current_pos != new_pos:
                if not dry_run:
                    cursor.execute(
                        "UPDATE players SET position = ? WHERE player_id = ?",
                        (new_pos, player_id)
                    )
                updated += 1
        else:
            skipped += 1

    if not dry_run:
        conn.commit()
    conn.close()

    return updated, skipped


def generate_sql_file(positions: dict[str, str], output_path: Path):
    """Generate SQL update statements file."""
    with open(output_path, "w") as f:
        f.write("-- Player Position Updates\n")
        f.write("-- Generated by enrich_player_data.py\n\n")
        f.write("BEGIN TRANSACTION;\n\n")

        for player_id, position in sorted(positions.items()):
            f.write(f"UPDATE players SET position='{position}' WHERE player_id='{player_id}';\n")

        f.write("\nCOMMIT;\n")

    print(f"SQL file written to: {output_path}")


def main():
    import argparse

    parser = argparse.ArgumentParser(description="Enrich player data from crawled sources")
    parser.add_argument("--dry-run", action="store_true", help="Preview changes without updating DB")
    parser.add_argument("--sql-only", action="store_true", help="Generate SQL file only")
    args = parser.parse_args()

    # Paths
    base_dir = Path(__file__).parent.parent
    data_dir = base_dir / "data" / "raw_crawled"
    db_path = base_dir / "data" / "wkbl.db"
    sql_path = base_dir / "data" / "position_updates.sql"

    print("=" * 50)
    print("Player Data Enrichment")
    print("=" * 50)

    # Collect positions from all sources
    print("\n[1/3] Extracting positions from data sources...")

    positions = {}

    # JSON sources
    json_positions = load_json_positions(data_dir)
    positions.update(json_positions)

    # HTML sources
    html_positions = parse_detailed_html(data_dir)
    positions.update(html_positions)

    print(f"\n  Total unique positions: {len(positions)}")

    # Position distribution
    dist = {"G": 0, "F": 0, "C": 0}
    for pos in positions.values():
        dist[pos] = dist.get(pos, 0) + 1
    print(f"  Distribution: G={dist['G']}, F={dist['F']}, C={dist['C']}")

    # Generate SQL file
    print("\n[2/3] Generating SQL file...")
    generate_sql_file(positions, sql_path)

    if args.sql_only:
        print("\n--sql-only mode: skipping database update")
        return

    # Update database
    print("\n[3/3] Updating database...")
    if args.dry_run:
        print("  (DRY RUN - no changes will be made)")

    updated, skipped = update_database(db_path, positions, dry_run=args.dry_run)

    print(f"\n  Updated: {updated} players")
    print(f"  Skipped (no data): {skipped} players")

    # Verify
    if not args.dry_run:
        conn = sqlite3.connect(db_path)
        cursor = conn.cursor()
        cursor.execute("SELECT COUNT(*) FROM players WHERE position IS NOT NULL")
        with_pos = cursor.fetchone()[0]
        cursor.execute("SELECT COUNT(*) FROM players")
        total = cursor.fetchone()[0]
        conn.close()

        print(f"\n  Verification: {with_pos}/{total} players now have position data")

    print("\n" + "=" * 50)
    print("Done!")


if __name__ == "__main__":
    main()
