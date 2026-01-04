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
  let transform v = if scope = PerGame then v /. gp_div else v in
  
  {
    team = totals.team;
    gp = totals.gp;
    min_total = transform totals.min_total;
    pts = transform (float totals.pts);
    margin = transform pts_margin;
    pts_against = transform pts_against;
    reb = transform (float totals.reb);
    ast = transform (float totals.ast);
    stl = transform (float totals.stl);
    blk = transform (float totals.blk);
    turnovers = transform (float totals.turnovers);
    fg_pct;
    fg3_pct;
    ft_pct;
    efg_pct;
    ts_pct;
    eff = transform eff_total;
  }