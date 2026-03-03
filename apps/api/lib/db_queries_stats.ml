(** Auto-generated from db_queries.ml split. *)

open Db_request
open Db_types
open Caqti_type

let leaders_base_stats = (t2 string string ->* leader_base) {|
  SELECT
    p.player_id,
    p.player_name,
    t.team_name_kr,
    COUNT(*) as gp,
    COALESCE(SUM(s.min_seconds), 0),
    COALESCE(SUM(s.pts), 0),
    COALESCE(SUM(s.reb_tot), 0),
    COALESCE(SUM(s.ast), 0),
    COALESCE(SUM(s.stl), 0),
    COALESCE(SUM(s.blk), 0),
    COALESCE(SUM(s.tov), 0),
    COALESCE(SUM(s.game_score) * 1.0, 0),
    COALESCE(SUM(s.fg_2p_m + s.fg_3p_m), 0),
    COALESCE(SUM(s.fg_2p_a + s.fg_3p_a), 0),
    COALESCE(SUM(s.fg_3p_m), 0),
    COALESCE(SUM(s.fg_3p_a), 0),
    COALESCE(SUM(s.ft_m), 0),
    COALESCE(SUM(s.ft_a), 0)
  FROM game_stats_clean s
  JOIN player_identities pi ON pi.player_id = s.player_id
  JOIN players p ON p.player_id = pi.canonical_player_id
  JOIN teams t ON s.team_code = t.team_code
  JOIN games g ON g.game_id = s.game_id
  WHERE (? = 'ALL' OR g.season_code = ?)
    AND g.game_type != '10'
  GROUP BY p.player_id, p.player_name, t.team_name_kr
|}


let leaders_base_cached = (string ->* leader_base) {|
  SELECT
    player_id,
    player_name,
    team_name_kr,
    gp,
    min_seconds,
    pts,
    reb,
    ast,
    stl,
    blk,
    tov,
    eff,
    fg_m,
    fg_a,
    fg3_m,
    fg3_a,
    ft_m,
    ft_a
  FROM leaders_base_cache
  WHERE season_code = ?
|}


let leaders_base_cached_all = (unit ->* leader_base) {|
  SELECT
    player_id,
    player_name,
    team_name_kr,
    COALESCE(SUM(gp), 0)::int,
    COALESCE(SUM(min_seconds), 0)::int,
    COALESCE(SUM(pts), 0)::int,
    COALESCE(SUM(reb), 0)::int,
    COALESCE(SUM(ast), 0)::int,
    COALESCE(SUM(stl), 0)::int,
    COALESCE(SUM(blk), 0)::int,
    COALESCE(SUM(tov), 0)::int,
    COALESCE(SUM(eff), 0)::float8,
    COALESCE(SUM(fg_m), 0)::int,
    COALESCE(SUM(fg_a), 0)::int,
    COALESCE(SUM(fg3_m), 0)::int,
    COALESCE(SUM(fg3_a), 0)::int,
    COALESCE(SUM(ft_m), 0)::int,
    COALESCE(SUM(ft_a), 0)::int
  FROM leaders_base_cache
  GROUP BY player_id, player_name, team_name_kr
|}


let leaders_pts = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.pts) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.pts) DESC LIMIT 5 |}


let leaders_pts_per36 = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.pts) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.pts) / SUM(s.min_seconds)) DESC LIMIT 5 |}


let leaders_reb = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.reb_tot) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.reb_tot) DESC LIMIT 5 |}


let leaders_reb_per36 = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.reb_tot) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.reb_tot) / SUM(s.min_seconds)) DESC LIMIT 5 |}


let leaders_ast = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.ast) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.ast) DESC LIMIT 5 |}


let leaders_ast_per36 = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.ast) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.ast) / SUM(s.min_seconds)) DESC LIMIT 5 |}


let leaders_stl = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.stl) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.stl) DESC LIMIT 5 |}


let leaders_stl_per36 = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.stl) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.stl) / SUM(s.min_seconds)) DESC LIMIT 5 |}


let leaders_blk = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.blk) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.blk) DESC LIMIT 5 |}


let leaders_blk_per36 = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.blk) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.blk) / SUM(s.min_seconds)) DESC LIMIT 5 |}

(* Leaders - extended (basketball-reference style) *)


let leaders_gp = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, COUNT(*) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr ORDER BY COUNT(*) DESC LIMIT 5 |}


let leaders_min = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.min_seconds) / 60.0 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.min_seconds) DESC LIMIT 5 |}


let leaders_min_totals = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.min_seconds) / 60.0 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr ORDER BY SUM(s.min_seconds) DESC LIMIT 5 |}


let leaders_pts_totals = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.pts) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.pts) DESC LIMIT 5 |}


let leaders_reb_totals = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.reb_tot) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.reb_tot) DESC LIMIT 5 |}


let leaders_ast_totals = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.ast) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.ast) DESC LIMIT 5 |}


let leaders_stl_totals = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.stl) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.stl) DESC LIMIT 5 |}


let leaders_blk_totals = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.blk) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.blk) DESC LIMIT 5 |}


let leaders_tov = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.tov) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.tov) DESC LIMIT 5 |}


let leaders_tov_per36 = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.tov) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.tov) / SUM(s.min_seconds)) DESC LIMIT 5 |}


let leaders_tov_totals = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.tov) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.tov) DESC LIMIT 5 |}


let leaders_eff = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.game_score) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.game_score) DESC LIMIT 5 |}


let leaders_eff_per36 = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.game_score) * 2160.0 / SUM(s.min_seconds)) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.game_score) / SUM(s.min_seconds)) DESC LIMIT 5 |}


let leaders_fg_pct = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.fg_2p_m + s.fg_3p_m) * 1.0) / NULLIF(SUM(s.fg_2p_a + s.fg_3p_a), 0) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.fg_2p_a + s.fg_3p_a) >= 50 ORDER BY ((SUM(s.fg_2p_m + s.fg_3p_m) * 1.0) / NULLIF(SUM(s.fg_2p_a + s.fg_3p_a), 0)) DESC LIMIT 5 |}


let leaders_fg3_pct = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.fg_3p_m) * 1.0) / NULLIF(SUM(s.fg_3p_a), 0) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.fg_3p_a) >= 20 ORDER BY ((SUM(s.fg_3p_m) * 1.0) / NULLIF(SUM(s.fg_3p_a), 0)) DESC LIMIT 5 |}


let leaders_ft_pct = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.ft_m) * 1.0) / NULLIF(SUM(s.ft_a), 0) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.ft_a) >= 20 ORDER BY ((SUM(s.ft_m) * 1.0) / NULLIF(SUM(s.ft_a), 0)) DESC LIMIT 5 |}


let leaders_ts_pct = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.pts) * 1.0) / NULLIF(2.0 * (SUM(s.fg_2p_a + s.fg_3p_a) + 0.44 * SUM(s.ft_a)), 0) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING (SUM(s.fg_2p_a + s.fg_3p_a) + SUM(s.ft_a)) >= 50 ORDER BY ((SUM(s.pts) * 1.0) / NULLIF(2.0 * (SUM(s.fg_2p_a + s.fg_3p_a) + 0.44 * SUM(s.ft_a)), 0)) DESC LIMIT 5 |}


let leaders_efg_pct = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, ((SUM(s.fg_2p_m + s.fg_3p_m) + 0.5 * SUM(s.fg_3p_m)) * 1.0) / NULLIF(SUM(s.fg_2p_a + s.fg_3p_a), 0) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.fg_2p_a + s.fg_3p_a) >= 50 ORDER BY (((SUM(s.fg_2p_m + s.fg_3p_m) + 0.5 * SUM(s.fg_3p_m)) * 1.0) / NULLIF(SUM(s.fg_2p_a + s.fg_3p_a), 0)) DESC LIMIT 5 |}


let leaders_usg_pct = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(u.usg_pct) FROM player_usg_stats u JOIN player_identities pi ON pi.player_id = u.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON u.team_code = t.team_code JOIN games g ON g.game_id = u.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' AND u.usg_pct IS NOT NULL GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 AND SUM(u.min_seconds) >= 6000 ORDER BY AVG(u.usg_pct) DESC LIMIT 5 |}

(** Position-filtered leader queries
    Position values: 'G' (Guard), 'F' (Forward), 'C' (Center), 'ALL' (no filter)
*)


let leaders_pts_by_position = (t2 (t2 string string) string ->* leader_entry) {|
  SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.pts)
  FROM game_stats_clean s
  JOIN player_identities pi ON pi.player_id = s.player_id
  JOIN players p ON p.player_id = pi.canonical_player_id
  JOIN teams t ON s.team_code = t.team_code
  JOIN games g ON g.game_id = s.game_id
  WHERE (? = 'ALL' OR g.season_code = ?)
    AND g.game_type != '10'
 AND (? = 'ALL' OR p.position LIKE ? || '%')
  GROUP BY p.player_id, p.player_name, t.team_name_kr
  HAVING COUNT(*) >= 5
  ORDER BY AVG(s.pts) DESC
  LIMIT 10
|}


let leaders_reb_by_position = (t2 (t2 string string) string ->* leader_entry) {|
  SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.reb_tot)
  FROM game_stats_clean s
  JOIN player_identities pi ON pi.player_id = s.player_id
  JOIN players p ON p.player_id = pi.canonical_player_id
  JOIN teams t ON s.team_code = t.team_code
  JOIN games g ON g.game_id = s.game_id
  WHERE (? = 'ALL' OR g.season_code = ?)
    AND g.game_type != '10'
 AND (? = 'ALL' OR p.position LIKE ? || '%')
  GROUP BY p.player_id, p.player_name, t.team_name_kr
  HAVING COUNT(*) >= 5
  ORDER BY AVG(s.reb_tot) DESC
  LIMIT 10
|}


let leaders_ast_by_position = (t2 (t2 string string) string ->* leader_entry) {|
  SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.ast)
  FROM game_stats_clean s
  JOIN player_identities pi ON pi.player_id = s.player_id
  JOIN players p ON p.player_id = pi.canonical_player_id
  JOIN teams t ON s.team_code = t.team_code
  JOIN games g ON g.game_id = s.game_id
  WHERE (? = 'ALL' OR g.season_code = ?)
    AND g.game_type != '10'
 AND (? = 'ALL' OR p.position LIKE ? || '%')
  GROUP BY p.player_id, p.player_name, t.team_name_kr
  HAVING COUNT(*) >= 5
  ORDER BY AVG(s.ast) DESC
  LIMIT 10
|}


let leaders_eff_by_position = (t2 (t2 string string) string ->* leader_entry) {|
  SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.game_score)
  FROM game_stats_clean s
  JOIN player_identities pi ON pi.player_id = s.player_id
  JOIN players p ON p.player_id = pi.canonical_player_id
  JOIN teams t ON s.team_code = t.team_code
  JOIN games g ON g.game_id = s.game_id
  WHERE (? = 'ALL' OR g.season_code = ?)
    AND g.game_type != '10'
 AND (? = 'ALL' OR p.position LIKE ? || '%')
  GROUP BY p.player_id, p.player_name, t.team_name_kr
  HAVING COUNT(*) >= 5
  ORDER BY AVG(s.game_score) DESC
  LIMIT 10
|}

(** Stat Awards (unofficial) *)


let stat_mvp_eff = (t2 string (t2 string int) ->* leader_entry) {|
  SELECT
    p.player_id,
    p.player_name,
    t.team_name_kr,
    AVG(s.game_score)
  FROM game_stats_clean s
  JOIN player_identities pi ON pi.player_id = s.player_id
  JOIN players p ON p.player_id = pi.canonical_player_id
  JOIN teams t ON s.team_code = t.team_code
  JOIN games g ON g.game_id = s.game_id
  WHERE (? = 'ALL' OR g.season_code = ?)
    AND g.game_type != '10'
 AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
  GROUP BY p.player_id, p.player_name, t.team_name_kr
  HAVING COUNT(*) >= 5
  ORDER BY AVG(s.game_score) DESC
  LIMIT 5
|}


let stat_mip_eff_delta = (t2 string (t2 int (t2 string int)) ->* leader_entry) {|
  WITH cur AS (
    SELECT
      pi.canonical_player_id AS player_id,
      AVG(s.game_score) AS eff_cur,
      COUNT(*) AS gp_cur,
      MAX(t.team_name_kr) AS team_name
    FROM game_stats_clean s
    JOIN player_identities pi ON pi.player_id = s.player_id
    JOIN games g ON g.game_id = s.game_id
    JOIN teams t ON t.team_code = s.team_code
    WHERE g.season_code = ?
      AND g.game_type != '10'
      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
    GROUP BY pi.canonical_player_id
  ),
  prev AS (
    SELECT
      pi.canonical_player_id AS player_id,
      AVG(s.game_score) AS eff_prev,
      COUNT(*) AS gp_prev
    FROM game_stats_clean s
    JOIN player_identities pi ON pi.player_id = s.player_id
    JOIN games g ON g.game_id = s.game_id
    WHERE g.season_code = ?
      AND g.game_type != '10'
      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
    GROUP BY pi.canonical_player_id
  )
  SELECT
    p.player_id,
    p.player_name,
    cur.team_name,
    (cur.eff_cur - prev.eff_prev) AS delta
  FROM cur
  JOIN prev ON prev.player_id = cur.player_id
  JOIN players p ON p.player_id = cur.player_id
  WHERE cur.gp_cur >= 10
    AND prev.gp_prev >= 10
  ORDER BY delta DESC
  LIMIT 5
|}


let mvp_race_candidates =
  let params = t2 string string in
  (params ->* mvp_candidate)
  {|
  WITH params AS (
    SELECT ? AS season
  ),
  team_records AS (
    SELECT
      CASE
        WHEN g.home_score > g.away_score THEN t_home.team_name_kr
        ELSE t_away.team_name_kr
      END AS winner,
      CASE
        WHEN g.home_score < g.away_score THEN t_home.team_name_kr
        ELSE t_away.team_name_kr
      END AS loser
    FROM games g
    JOIN teams t_home ON g.home_team_code = t_home.team_code
    JOIN teams t_away ON g.away_team_code = t_away.team_code
    CROSS JOIN params p
    WHERE g.home_score IS NOT NULL AND g.away_score IS NOT NULL
      AND (p.season = 'ALL' OR g.season_code = p.season)
  ),
  team_standings AS (
    SELECT
      team_name,
      SUM(CASE WHEN is_win THEN 1 ELSE 0 END) AS wins,
      SUM(CASE WHEN NOT is_win THEN 1 ELSE 0 END) AS losses
    FROM (
      SELECT winner AS team_name, TRUE AS is_win FROM team_records
      UNION ALL
      SELECT loser AS team_name, FALSE AS is_win FROM team_records
    ) combined
    GROUP BY team_name
  ),
  player_aggs AS (
    SELECT
      gs.player_id,
      p.player_name,
      t.team_name_kr AS team_name,
      COUNT(DISTINCT gs.game_id) AS gp,
      ROUND(CAST(SUM(gs.pts) AS NUMERIC) / NULLIF(COUNT(DISTINCT gs.game_id), 0), 1) AS ppg,
      ROUND(CAST(SUM(gs.reb_tot) AS NUMERIC) / NULLIF(COUNT(DISTINCT gs.game_id), 0), 1) AS rpg,
      ROUND(CAST(SUM(gs.ast) AS NUMERIC) / NULLIF(COUNT(DISTINCT gs.game_id), 0), 1) AS apg,
      ROUND(CAST(SUM(gs.stl) AS NUMERIC) / NULLIF(COUNT(DISTINCT gs.game_id), 0), 1) AS spg,
      ROUND(CAST(SUM(gs.blk) AS NUMERIC) / NULLIF(COUNT(DISTINCT gs.game_id), 0), 1) AS bpg,
      ROUND(
        CAST(SUM(gs.pts + gs.reb_tot + gs.ast + gs.stl + gs.blk - (gs.fg_2p_a + gs.fg_3p_a - gs.fg_2p_m - gs.fg_3p_m) - (gs.ft_a - gs.ft_m) - gs.tov) AS NUMERIC)
        / NULLIF(COUNT(DISTINCT gs.game_id), 0),
        1
      ) AS efficiency
    FROM game_stats_clean gs
    JOIN players p ON gs.player_id = p.player_id
    JOIN teams t ON gs.team_code = t.team_code
    JOIN games g ON gs.game_id = g.game_id
    CROSS JOIN params pp
    WHERE (pp.season = 'ALL' OR g.season_code = pp.season)
      AND gs.min_seconds > 0
    GROUP BY gs.player_id, p.player_name, t.team_name_kr
    HAVING COUNT(DISTINCT gs.game_id) >= 5
  )
  SELECT
    pa.player_id,
    pa.player_name,
    pa.team_name,
    pa.gp,
    pa.ppg,
    pa.rpg,
    pa.apg,
    pa.spg,
    pa.bpg,
    pa.efficiency,
    COALESCE(ts.wins, 0) AS team_wins,
    COALESCE(ts.losses, 0) AS team_losses,
    ROUND(
      CASE WHEN (COALESCE(ts.wins, 0) + COALESCE(ts.losses, 0)) > 0
        THEN CAST(COALESCE(ts.wins, 0) AS NUMERIC) / (COALESCE(ts.wins, 0) + COALESCE(ts.losses, 0))
        ELSE 0
      END,
      3
    ) AS team_win_pct
  FROM player_aggs pa
  LEFT JOIN team_standings ts ON pa.team_name = ts.team_name
  WHERE pa.gp >= ?
  ORDER BY
    (pa.ppg * 2 + pa.rpg * 1.2 + pa.apg * 1.5 + pa.spg * 2 + pa.bpg * 2 + pa.efficiency * 0.5
     + CASE WHEN (COALESCE(ts.wins, 0) + COALESCE(ts.losses, 0)) > 0
         THEN (CAST(COALESCE(ts.wins, 0) AS NUMERIC) / (COALESCE(ts.wins, 0) + COALESCE(ts.losses, 0))) * 20
         ELSE 0
       END) DESC
  LIMIT 20
|}

(** Get all Q4 PBP events for clutch time analysis
    Clutch time = Q4 + clock <= 5:00 + score diff <= 5 *)


let clutch_pbp_events = (string ->* pbp_event) {|
  SELECT
    period_code, event_index, team_side, description,
    team1_score, team2_score, clock
  FROM play_by_play_events p
  JOIN games g ON p.game_id = g.game_id
  WHERE period_code = 'Q4'
    AND (
      CASE
        WHEN clock ~ '^\d+:\d+$' THEN
          CAST(SPLIT_PART(clock, ':', 1) AS INTEGER) * 60 +
          CAST(SPLIT_PART(clock, ':', 2) AS INTEGER)
        ELSE 600
      END
    ) <= 300
    AND ABS(COALESCE(team1_score, 0) - COALESCE(team2_score, 0)) <= 5
    AND ($1 = 'ALL' OR g.season_code = $1)
  ORDER BY p.game_id, event_index
|}

(** Aggregated clutch time statistics per player
    Parses description for scoring events:
    - "N점슛성공" = field goal made (N=2 or 3)
    - "N점슛실패" = field goal attempted but missed
    - "자유투성공" = free throw made
    - "자유투실패" = free throw missed *)


let clutch_stats_by_season = (string ->* clutch_stats) {|
  WITH clutch_events AS (
    SELECT
      COALESCE(pi.canonical_player_id, p.player_id) AS player_id,
      p.game_id,
      p.description,
      CASE
        WHEN p.description ~ '2점슛성공' THEN 2
     WHEN p.description ~ '3점슛성공' THEN 3
     WHEN p.description ~ '자유투성공' THEN 1
     ELSE 0
   END AS points_scored,
   CASE WHEN p.description ~ '[23]점슛성공' THEN 1 ELSE 0 END AS fg_made,
   CASE WHEN p.description ~ '[23]점슛' THEN 1 ELSE 0 END AS fg_att,
      CASE WHEN p.description ~ '3점슛성공' THEN 1 ELSE 0 END AS three_made,
      CASE WHEN p.description ~ '자유투성공' THEN 1 ELSE 0 END AS ft_made,
      CASE WHEN p.description ~ '자유투' THEN 1 ELSE 0 END AS ft_att
    FROM play_by_play_events p
    LEFT JOIN player_identities pi ON pi.player_id = p.player_id
    JOIN games g ON p.game_id = g.game_id
    WHERE p.period_code = 'Q4'
      AND p.player_id IS NOT NULL
   AND (
     CASE
       WHEN p.clock ~ '^\d+:\d+$' THEN
         CAST(SPLIT_PART(p.clock, ':', 1) AS INTEGER) * 60 +
         CAST(SPLIT_PART(p.clock, ':', 2) AS INTEGER)
       ELSE 600
     END
   ) <= 300
   AND ABS(COALESCE(p.team1_score, 0) - COALESCE(p.team2_score, 0)) <= 5
   AND ($1 = 'ALL' OR g.season_code = $1)
   AND g.game_type != '10'
  )
  SELECT
 ce.player_id,
 pl.player_name,
 COALESCE(t.team_name_kr, 'Unknown') AS team_name,
 COUNT(DISTINCT ce.game_id) AS clutch_games,
 SUM(ce.points_scored) AS clutch_points,
 SUM(ce.fg_made) AS clutch_fg_made,
 SUM(ce.fg_att) AS clutch_fg_att,
 CASE
   WHEN SUM(ce.fg_att) > 0
   THEN CAST(SUM(ce.fg_made) AS FLOAT) / SUM(ce.fg_att)
   ELSE 0.0
 END AS clutch_fg_pct,
 SUM(ce.ft_made) AS clutch_ft_made,
 SUM(ce.ft_att) AS clutch_ft_att,
 SUM(ce.three_made) AS clutch_3p_made
  FROM clutch_events ce
  JOIN players pl ON ce.player_id = pl.player_id
  LEFT JOIN (
    SELECT DISTINCT ON (pi2.canonical_player_id)
      pi2.canonical_player_id AS player_id,
      gs.team_code
    FROM game_stats_clean gs
    JOIN player_identities pi2 ON pi2.player_id = gs.player_id
    JOIN games g2 ON gs.game_id = g2.game_id
    WHERE ($1 = 'ALL' OR g2.season_code = $1)
    ORDER BY pi2.canonical_player_id, g2.game_date DESC, g2.game_id DESC
  ) latest_team ON ce.player_id = latest_team.player_id
  LEFT JOIN teams t ON latest_team.team_code = t.team_code
  GROUP BY ce.player_id, pl.player_name, t.team_name_kr
  HAVING SUM(ce.points_scored) > 0 OR SUM(ce.fg_att) > 0 OR SUM(ce.ft_att) > 0
  ORDER BY SUM(ce.points_scored) DESC, clutch_fg_pct DESC
  LIMIT 50
|}

(* On/Off Impact Queries *)


let on_off_impact_stats = (t2 string string ->* (t2 string (t2 string (t2 string (t2 int (t2 int (t2 int int))))))) {|
  SELECT
    p.player_id,
    p.player_name,
    t.team_name_kr,
    COUNT(DISTINCT s.game_id) as games_played,
    COALESCE(SUM(s.min_seconds), 0) as total_min_seconds,
    COALESCE(SUM(pm.plus_minus), 0) as total_plus_minus,
    COUNT(pm.plus_minus) as games_with_pm
  FROM game_stats_clean s
  JOIN players p ON s.player_id = p.player_id
  JOIN teams t ON s.team_code = t.team_code
  JOIN games g ON g.game_id = s.game_id
  LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
  WHERE ($1 = 'ALL' OR g.season_code = $1)
    AND g.game_type != '10'
    AND s.min_seconds > 0
  GROUP BY p.player_id, p.player_name, t.team_name_kr
  HAVING COUNT(DISTINCT s.game_id) >= 5
    AND COALESCE(SUM(s.min_seconds), 0) / 60.0 >= 50
  ORDER BY
    CASE WHEN COUNT(pm.plus_minus) > 0
      THEN CAST(COALESCE(SUM(pm.plus_minus), 0) AS FLOAT) / COUNT(pm.plus_minus)
      ELSE -999
    END DESC
  LIMIT CASE WHEN $2 = '' THEN 50 ELSE CAST($2 AS INTEGER) END
|}


let on_off_impact_for_player = (t2 string string ->? (t2 string (t2 string (t2 string (t2 int (t2 int (t2 int int))))))) {|
  SELECT
    p.player_id,
    p.player_name,
    t.team_name_kr,
    COUNT(DISTINCT s.game_id) as games_played,
    COALESCE(SUM(s.min_seconds), 0) as total_min_seconds,
    COALESCE(SUM(pm.plus_minus), 0) as total_plus_minus,
    COUNT(pm.plus_minus) as games_with_pm
  FROM game_stats_clean s
  JOIN players p ON s.player_id = p.player_id
  JOIN teams t ON s.team_code = t.team_code
  JOIN games g ON g.game_id = s.game_id
  LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
  WHERE s.player_id = $1
    AND ($2 = 'ALL' OR g.season_code = $2)
    AND g.game_type != '10'
    AND s.min_seconds > 0
  GROUP BY p.player_id, p.player_name, t.team_name_kr
|}

(* Shot chart: Get aggregated shot stats by zone for a player *)


let player_shot_stats = (t2 string string ->* t4 string int int float) {|
  WITH canon AS (
    SELECT COALESCE(
      (SELECT canonical_player_id FROM player_identities WHERE player_id = $1),
      $1
    ) AS canonical_player_id
  )
  SELECT
    CASE
      WHEN description LIKE '%페인트존%' THEN 'paint'
      WHEN description LIKE '%3점슛%' THEN 'three'
      WHEN description LIKE '%2점슛%' THEN 'mid'
      ELSE 'other'
    END as zone,
    SUM(CASE WHEN description LIKE '%성공%' THEN 1 ELSE 0 END)::int as made,
    COUNT(*)::int as attempts,
    ROUND(
      SUM(CASE WHEN description LIKE '%성공%' THEN 1 ELSE 0 END)::numeric /
      NULLIF(COUNT(*), 0) * 100, 1
    )::float as pct
  FROM play_by_play_events p
  LEFT JOIN player_identities pi ON pi.player_id = p.player_id
  JOIN games g ON p.game_id = g.game_id
  WHERE COALESCE(pi.canonical_player_id, p.player_id) = (SELECT canonical_player_id FROM canon)
    AND p.description LIKE '%슛%'
    AND ($2 = 'ALL' OR g.season_code = $2)
  GROUP BY 1
  ORDER BY 1
|}

(* Shot chart: Get player info for shot chart header *)


let player_shot_info = (string ->? t3 string string string) {|
  WITH canon AS (
    SELECT COALESCE(
      (SELECT canonical_player_id FROM player_identities WHERE player_id = $1),
      $1
    ) AS canonical_player_id
  ),
  latest_gs AS (
    SELECT
      pi.canonical_player_id AS player_id,
      gs.team_code,
      g.season_code,
      g.game_date,
      g.game_id
    FROM game_stats_clean gs
    JOIN player_identities pi ON pi.player_id = gs.player_id
    JOIN games g ON gs.game_id = g.game_id
    WHERE pi.canonical_player_id = (SELECT canonical_player_id FROM canon)
    ORDER BY g.season_code DESC, g.game_date DESC, g.game_id DESC
    LIMIT 1
  )
  SELECT
    p.player_id,
    p.player_name,
    COALESCE(t.team_name_kr, 'Unknown')
  FROM players p
  LEFT JOIN latest_gs ON p.player_id = latest_gs.player_id
  LEFT JOIN teams t ON latest_gs.team_code = t.team_code
  WHERE p.player_id = (SELECT canonical_player_id FROM canon)
  LIMIT 1
|}

(** Quarter scores from PBP for game flow analysis
    Note: In PBP data, team2 = HOME, team1 = AWAY (verified pattern) *)


let quarter_scores_by_game = (string ->* t3 string int int) {|
  SELECT period_code,
         COALESCE(MAX(team2_score), 0) as home_score,
         COALESCE(MAX(team1_score), 0) as away_score
  FROM play_by_play_events
  WHERE game_id = ? AND team1_score IS NOT NULL
  GROUP BY period_code
  ORDER BY period_code
|}

(** PBP data quality verification
    Returns: (pattern, count) where pattern is T2=HOME, SCORE_MISSING, or MISMATCH
    Note: Properly handles NULL scores (common in older seasons) *)

