# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.4.0] - 2026-02-19

152 commits since v0.3.0. Major redesign, live features, and codebase decomposition.

### Added

- Basketball Reference-level redesign (Phases 1-6): design system, FK fix, SEO, defense-in-depth URL encoding
- Official awards scraping pipeline
- WKBL official site data scraper
- DataLab sync with server-side automation
- Live score polling and broadcasting
- Live pulsing animation and dynamic OG images
- Predict insights with spread/total, radar chart visualization
- Smart pagination for games and history views
- Pace and advanced stats (USG%, BPG leader)
- Dark mode toggle with header navigation and standings column
- Language toggle with Korean-localized pages and copy
- Sentry and Clarity observability scripts
- CSV export with table sorting and HTTP cache headers
- CSP nonce, JS extraction, backtrace handling, env config (security audit phase 2)
- Compression, 404 handling, cache busting, exception narrowing (quality audit phase 1)
- Cross-linking enhancement and table design unification
- Row-clickable team tables
- Mobile hamburger menu
- Team logo scaling and win-rate progress bar
- Live text coverage and missing games QA view
- Schedule coverage by season QA tooling
- Identity duplicate checks and schedule missing checks
- Player disambiguation across leaders and rosters
- CI: build and test on pull requests

### Changed

- `db.ml` split into 9 focused layer modules (7,032 to 1,374 lines)
- `main.ml` split into 14 focused modules (2,746 to 398 lines)
- Cache module extracted to `db_cache.ml`
- `live_game` moved to domain layer with scraper skeleton
- `h2h_game` field renaming completed
- URL encoding centralized via href helpers across all views
- CDN removed, Chart.js lazy-loaded
- Breadcrumbs consolidated, season label wrapping fixed
- Dead code removed from views, team footers added
- Boxscore chips compacted on mobile
- Edge caching via s-maxage and stale-while-revalidate for Cloudflare
- Cache TTL increased with parallelized season page queries
- Production sync hardened (v2): stability and test improvements
- Sigma totals marker replaced with plain text
- Shell-out removed for ImageMagick execution
- Curl shell-out removed from OCaml, list ops hardened
- Tailwind CSS committed for production deployment
- `.worktrees` untracked, agent runtime dirs added to gitignore

### Fixed

- Scheduled games placeholder handling (0-0 scores treated as pregame)
- Language cookie parsing
- Player canonical links and duplicate ID canonicalization
- 0-0 score handling in boxscore, games_calc view, team pages, and live widget
- Boxscore sync edge cases: oneshot queries to avoid prepared statement collisions
- Schedule sync retry when zero rows, materialized view refresh, partial scrape hardening
- FK violation for team_code 81 (Team Unibl)
- Column name typos (`season` to `season_code` in index creation)
- RNG initialization crash in main.ml
- URL encoding: `pct_encode` used instead of `escape_html` for hrefs
- PBP backfill made atomic to avoid partial quarters
- PBP backfill async to avoid timeouts
- PBP refresh for same-day games and incomplete play-by-play
- Invalid season falls back to latest
- `woman.win` redirects to `wkbl.win`
- HTMX and core scripts restored after breakage
- Row-click cursor preserved after HTMX swaps
- Duplicate player rows collapsed in list view
- Bad boxscore rows excluded
- Team links clickable with correct score flow and empty states
- Season dropdown facts and data freshness
- Data pipeline restored with OCaml scraper
- Legends data restored, live scraper hardened
- dune-build-info dependency for Docker build
- Lineups DB query errors (team_name, plus_minus, season_id)
- Build-breaking trailing semicolon in scraper_tool
- Layout, overflow, and accessibility fixes (visual audit)
- Accessibility: contrast, headings, table scope, alt text on player images
- Team column width in standings tables
- Games season selector
- PBP language propagation

## [0.3.0] - 2026-01-30

### Added

- Initial tracked release.
