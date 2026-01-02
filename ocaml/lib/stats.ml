(** Pure stat computations for WKBL aggregates. *)

open Domain

let round1 value =
  Float.round (value *. 10.) /. 10.

let pct made att =
  if att <= 0.0 then 0.0 else round1 ((made /. att) *. 100.0)

let ts_pct pts fg_a ft_a =
  let denom = 2.0 *. (fg_a +. 0.44 *. ft_a) in
  if denom <= 0.0 then 0.0 else round1 ((pts /. denom) *. 100.0)

let eff pts reb ast stl blk turnovers fg_m fg_a ft_m ft_a =
  pts
  +. reb
  +. ast
  +. stl
  +. blk
  -. turnovers
  -. ((fg_a -. fg_m) +. (ft_a -. ft_m))

let team_stats_of_totals ~scope ~(margin: team_margin option) (totals: team_totals) =
  let gp = totals.gp in
  let scale =
    match scope with
    | PerGame -> if gp > 0 then 1.0 /. float_of_int gp else 1.0
    | Totals -> 1.0
  in
  let to_f value = float_of_int value *. scale in
  let min_total =
    match scope with
    | PerGame -> if gp > 0 then totals.min_total /. float_of_int gp else 0.0
    | Totals -> totals.min_total
  in
  let fg2_m = to_f totals.fg2_m in
  let fg2_a = to_f totals.fg2_a in
  let fg3_m = to_f totals.fg3_m in
  let fg3_a = to_f totals.fg3_a in
  let ft_m = to_f totals.ft_m in
  let ft_a = to_f totals.ft_a in
  let fg_m = fg2_m +. fg3_m in
  let fg_a = fg2_a +. fg3_a in
  let pts = to_f totals.pts in
  let reb = to_f totals.reb in
  let reb_off = to_f totals.reb_off in
  let reb_def = to_f totals.reb_def in
  let ast = to_f totals.ast in
  let stl = to_f totals.stl in
  let blk = to_f totals.blk in
  let turnovers = to_f totals.turnovers in
  let pts_against =
    match margin with
    | None -> 0.0
    | Some value ->
        let raw = float_of_int value.pts_against in
        raw *. scale
  in
  let margin_value =
    match scope with
    | PerGame -> pts -. pts_against
    | Totals ->
        match margin with
        | None -> pts -. pts_against
        | Some value -> float_of_int (value.pts_for - value.pts_against)
  in
  {
    season = totals.season;
    team = totals.team;
    gp;
    min_total;
    pts;
    margin = margin_value;
    pts_against;
    reb;
    reb_off;
    reb_def;
    ast;
    stl;
    blk;
    turnovers;
    fg_pct = pct fg_m fg_a;
    fg3_pct = pct fg3_m fg3_a;
    ft_pct = pct ft_m ft_a;
    efg_pct = pct (fg_m +. 0.5 *. fg3_m) fg_a;
    ts_pct = ts_pct pts fg_a ft_a;
    eff = eff pts reb ast stl blk turnovers fg_m fg_a ft_m ft_a;
  }
