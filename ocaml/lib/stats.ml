(** Statistical Calculation Logic

    Principles:
    - Pure functions for metric conversion
    - Handle division by zero
    - Conversion between Totals and Per Game
*)

open Domain

(** Main logic to convert database totals into formatted team stats *)
let team_stats_of_totals ~(scope: team_scope) ~(margin: team_margin option) (totals: team_totals) : team_stats =
  let gp_f = float totals.gp in
  let gp_div = if gp_f = 0.0 then 1.0 else gp_f in
  
  (* Calculate derived metrics *)
  let fg_m = totals.fg2_m + totals.fg3_m in
  let fg_a = totals.fg2_a + totals.fg3_a in
  
  let pct m a = if a = 0 then 0.0 else float m /. float a *. 100.0 in
  
  let fg_pct = pct fg_m fg_a in
  let fg3_pct = pct totals.fg3_m totals.fg3_a in
  let ft_pct = pct totals.ft_m totals.ft_a in
  
  let efg_pct =
    if fg_a = 0 then 0.0
    else (float fg_m +. 0.5 *. float totals.fg3_m) /. float fg_a *. 100.0
  in
  
  let ts_pct =
    let denom = 2.0 *. (float fg_a +. 0.44 *. float totals.ft_a) in
    if denom = 0.0 then 0.0 else float totals.pts /. denom *. 100.0
  in
  
  (* NBA Efficiency Formula *)
  let eff_total =
    float (totals.pts + totals.reb + totals.ast + totals.stl + totals.blk)
    -. float (totals.fg2_a - totals.fg2_m + totals.fg3_a - totals.fg3_m + totals.ft_a - totals.ft_m + totals.turnovers)
  in

  let pts_margin =
    match margin with
    | Some m -> float (m.pts_for - m.pts_against)
    | None -> 0.0
  in
  
  let pts_against =
    match margin with
    | Some m -> float m.pts_against
    | None -> 0.0
  in

  (* Scope conversion *)
  let transform_total v = if scope = PerGame then v /. gp_div else v in
  let transform_margin v =
    match scope, margin with
    | PerGame, Some m ->
        let gp_m = float m.gp in
        let gp_m_div = if gp_m = 0.0 then gp_div else gp_m in
        v /. gp_m_div
    | PerGame, None -> v /. gp_div
    | Totals, _ -> v
  in
  
  {
    team = totals.team;
    gp = totals.gp;
    min_total = transform_total totals.min_total;
    pts = transform_total (float totals.pts);
    margin = transform_margin pts_margin;
    pts_against = transform_margin pts_against;
    reb = transform_total (float totals.reb);
    ast = transform_total (float totals.ast);
    stl = transform_total (float totals.stl);
    blk = transform_total (float totals.blk);
    turnovers = transform_total (float totals.turnovers);
    fg_pct;
    fg3_pct;
    ft_pct;
    efg_pct;
    ts_pct;
    eff = transform_total eff_total;
  }

(** Player Efficiency Rating (PER) - Simplified Version

    Since we don't have FGA/FTA breakdown, we use a simplified formula:
    PER = (EFF / MIN) * 48 * pace_adjustment

    Normalized so league average = 15.0
*)

(** Calculate per-minute efficiency and convert to PER scale *)
let calculate_per ~total_minutes ~efficiency : float =
  if total_minutes <= 0.0 then 0.0
  else
    (* Per-minute efficiency *)
    let per_min = efficiency /. total_minutes in
    (* Scale to per-48-minutes (full game) *)
    let per_48 = per_min *. 48.0 in
    (* WKBL uses 40-min games, adjust factor *)
    let pace_factor = 40.0 /. 48.0 in
    (* Normalize to ~15 average (empirical adjustment for WKBL data) *)
    let normalized = per_48 *. pace_factor *. 1.2 in
    (* Clamp to reasonable range *)
    max 0.0 (min 40.0 normalized)

(** Calculate PER from player aggregate data *)
let per_of_player_aggregate (p : Domain.player_aggregate) : float =
  calculate_per ~total_minutes:p.total_minutes ~efficiency:p.efficiency

(** Four Factors - Team Performance Metrics (Dean Oliver)

    1. eFG% - Effective Field Goal Percentage (shooting)
    2. TOV% - Turnover Percentage (ball security)
    3. ORB% - Offensive Rebound Percentage (second chances)
    4. FTR  - Free Throw Rate (getting to the line)
*)

type four_factors = {
  efg_pct: float;   (** Effective FG% - already in team_stats *)
  tov_pct: float;   (** Turnovers per 100 possessions *)
  orb_pct: float;   (** Offensive rebound rate *)
  ftr: float;       (** FT attempts / FG attempts *)
}

(** Calculate Four Factors from team totals *)
let four_factors_of_totals (t : Domain.team_totals) : four_factors =
  let fg_a = t.fg2_a + t.fg3_a in
  let fg_m = t.fg2_m + t.fg3_m in

  (* eFG% = (FGM + 0.5 * 3PM) / FGA *)
  let efg_pct =
    if fg_a = 0 then 0.0
    else (float fg_m +. 0.5 *. float t.fg3_m) /. float fg_a *. 100.0
  in

  (* Possessions estimate: FGA + 0.44*FTA + TOV - ORB *)
  let poss = float fg_a +. 0.44 *. float t.ft_a +. float t.turnovers -. float t.reb_off in
  let poss = max poss 1.0 in

  (* TOV% = TOV / Possessions * 100 *)
  let tov_pct = float t.turnovers /. poss *. 100.0 in

  (* ORB% = ORB / (ORB + Opp_DRB) - we don't have opponent DRB, use estimate *)
  (* Simplified: ORB / REB * 100 as proxy *)
  let orb_pct =
    if t.reb = 0 then 0.0
    else float t.reb_off /. float t.reb *. 100.0
  in

  (* FTR = FTA / FGA *)
  let ftr =
    if fg_a = 0 then 0.0
    else float t.ft_a /. float fg_a *. 100.0
  in

  { efg_pct; tov_pct; orb_pct; ftr }

(** ===== Advanced Player Statistics ===== *)

(** Calculate True Shooting Percentage
    TS% = PTS / (2 * (FGA + 0.44 * FTA)) * 100
*)
let true_shooting_pct ~pts ~fga ~fta : float =
  let denom = 2.0 *. (float_of_int fga +. 0.44 *. float_of_int fta) in
  if denom <= 0.0 then 0.0
  else float_of_int pts /. denom *. 100.0

(** Calculate Effective Field Goal Percentage
    eFG% = (FGM + 0.5 * 3PM) / FGA * 100
*)
let effective_fg_pct ~fg_made ~fg3_made ~fga : float =
  if fga <= 0 then 0.0
  else (float_of_int fg_made +. 0.5 *. float_of_int fg3_made) /. float_of_int fga *. 100.0

(** Calculate Usage Rate
    USG% = (FGA + 0.44 * FTA + TOV) / Team_Possessions * 100

    Note: Requires team possession estimate. If not available, uses player's
    share of team minutes as a proxy.
*)
let usage_rate ~fga ~fta ~tov ~team_poss : float =
  let player_poss = float_of_int fga +. 0.44 *. float_of_int fta +. float_of_int tov in
  if team_poss <= 0.0 then 0.0
  else player_poss /. team_poss *. 100.0

(** Estimate team possessions using standard formula
    Poss ≈ FGA + 0.44*FTA + TOV - OREB
*)
let estimate_team_possessions ~team_fga ~team_fta ~team_tov ~team_oreb : float =
  let poss = float_of_int team_fga +. 0.44 *. float_of_int team_fta
             +. float_of_int team_tov -. float_of_int team_oreb in
  max poss 1.0 (* Avoid division by zero *)

(** Calculate Offensive Rating (points scored per 100 possessions)
    ORtg = (PTS / Possessions) * 100
*)
let offensive_rating ~pts ~possessions : float =
  if possessions <= 0.0 then 0.0
  else float_of_int pts /. possessions *. 100.0

(** Calculate Defensive Rating (points allowed per 100 possessions)
    DRtg = (Opp_PTS / Possessions) * 100
*)
let defensive_rating ~opp_pts ~possessions : float =
  if possessions <= 0.0 then 0.0
  else float_of_int opp_pts /. possessions *. 100.0

(** Calculate Net Rating
    NetRtg = ORtg - DRtg
*)
let net_rating ~ortg ~drtg : float = ortg -. drtg

(** Calculate all advanced stats from shooting stats and team context *)
let advanced_stats_of_shooting
    ~(shooting: Domain.player_shooting_stats)
    ~(aggregate: Domain.player_aggregate)
    ~(team_totals: Domain.team_totals option)
    : Domain.player_advanced_stats =
  let pts = aggregate.total_points in
  let fga = shooting.pss_fg_attempted in
  let fta = shooting.pss_ft_attempted in
  let tov = aggregate.total_turnovers in
  let fg_made = shooting.pss_fg_made in
  let fg3_made = shooting.pss_fg3_made in

  let ts_pct = true_shooting_pct ~pts ~fga ~fta in
  let efg_pct = effective_fg_pct ~fg_made ~fg3_made ~fga in
  let per = calculate_per ~total_minutes:aggregate.total_minutes ~efficiency:aggregate.efficiency in

  (* Calculate usage rate if team totals available *)
  let usage_pct, ortg, drtg =
    match team_totals with
    | Some tt ->
        let team_fga = tt.fg2_a + tt.fg3_a in
        let team_poss = estimate_team_possessions
          ~team_fga ~team_fta:tt.ft_a ~team_tov:tt.turnovers ~team_oreb:tt.reb_off in
        (* Scale player's usage by their share of team games *)
        let games_ratio = float_of_int aggregate.games_played /. float_of_int (max 1 tt.gp) in
        let player_poss = (float_of_int fga +. 0.44 *. float_of_int fta +. float_of_int tov) in
        let scaled_team_poss = team_poss *. games_ratio in
        let usg = if scaled_team_poss <= 0.0 then 0.0
                  else player_poss /. scaled_team_poss *. 100.0 in
        (* Simplified ORtg/DRtg using team rates as proxy *)
        let team_ortg = offensive_rating ~pts:tt.pts ~possessions:team_poss in
        let team_drtg = 100.0 in (* Default estimate without opponent data *)
        (usg, team_ortg, team_drtg)
    | None ->
        (0.0, 0.0, 0.0)
  in

  {
    pas_player_id = shooting.pss_player_id;
    pas_name = shooting.pss_name;
    pas_team = aggregate.team_name;
    pas_games = aggregate.games_played;
    pas_ts_pct = ts_pct;
    pas_efg_pct = efg_pct;
    pas_usage_pct = usage_pct;
    pas_per = per;
    pas_ortg = ortg;
    pas_drtg = drtg;
    pas_net_rtg = net_rating ~ortg ~drtg;
  }
