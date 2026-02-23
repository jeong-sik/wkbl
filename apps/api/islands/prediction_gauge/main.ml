(** Prediction-gauge Wasm island.
    Interactive probability gauge for match predictions.
    Reads pre-computed probabilities from data-* attributes
    (sourced from prediction_breakdown), renders animated gauge,
    and supports home/neutral toggle with client-side re-computation.

    Required data attributes on container:
    - data-home-name, data-away-name
    - data-home-elo, data-away-elo (Elo ratings)
    - data-pyth-prob (Pythagorean probability, venue-independent)
    - data-stats-prob (Stats probability, includes home advantage if applicable)
    - data-h2h-prob (head-to-head probability)
    - data-is-neutral ("true"/"false")
    Marks container data-island-hydrated="true" *)

open Js_of_ocaml
module Pc = Wkbl_island_core.Prediction_calc

(* ── JS interop helpers ────────────────────── *)

let query_all (root : #Dom.node Js.t) (sel : string) : Dom.node Js.t list =
  let nl = (Js.Unsafe.coerce root)##querySelectorAll (Js.string sel) in
  let n : int = nl##.length in
  List.init n (fun i -> Js.Opt.get (nl##item i) (fun () -> assert false))

let to_element (n : Dom.node Js.t) : Dom_html.element Js.t =
  Js.Opt.get (Dom_html.CoerceTo.element n) (fun () -> assert false)

let get_attr (el : Dom_html.element Js.t) (name : string) : string option =
  let v = el##getAttribute (Js.string name) in
  Js.Opt.to_option (Js.Opt.map v Js.to_string)

let get_float (el : Dom_html.element Js.t) (name : string) ~(default : float) : float =
  match get_attr el name with
  | Some s -> (match float_of_string_opt s with Some f -> f | None -> default)
  | None -> default

let set_inner_html (el : Dom_html.element Js.t) (html : string) : unit =
  el##.innerHTML := Js.string html

(* ── Prediction state ──────────────────────── *)

type gauge_state = {
  home_name : string;
  away_name : string;
  home_elo : float;
  away_elo : float;
  pyth_prob : float;         (** Pythagorean probability (venue-independent, constant) *)
  stats_relative : float;    (** Stats relative_strength (without home_advantage) *)
  h2h_prob : float;          (** Head-to-head probability (constant) *)
  mutable is_neutral : bool;
}

(* ── Compute prediction ────────────────────── *)

let compute (st : gauge_state) =
  let home_adv = if st.is_neutral then 0.0 else 65.0 in
  let elo_prob =
    Pc.elo_expected ~home_rating:st.home_elo ~away_rating:st.away_elo ~home_adv
  in
  let pyth_prob = st.pyth_prob in
  let stats_home_adv = if st.is_neutral then 0.0 else Pc.home_court_boost in
  let stats_prob = Pc.clamp01 (st.stats_relative +. stats_home_adv) in
  let final_prob =
    Pc.blend_probabilities ~elo_prob ~pyth_prob ~stats_prob ~h2h_prob:st.h2h_prob
  in
  let margin = Pc.predict_margin ~prob_a:final_prob in
  (final_prob, margin, elo_prob, pyth_prob, stats_prob)

(* ── Render gauge HTML ─────────────────────── *)

let render_gauge (container : Dom_html.element Js.t) (st : gauge_state) =
  let (prob_home, margin, elo_prob, pyth_prob, stats_prob) = compute st in
  let home_pct = prob_home *. 100.0 in
  let away_pct = (1.0 -. prob_home) *. 100.0 in
  let badge = Pc.margin_badge margin in
  let abs_margin = abs_float margin in
  let winner = if prob_home >= 0.5 then st.home_name else st.away_name in
  let winner_cls =
    if prob_home >= 0.5 then "text-orange-700 dark:text-orange-400"
    else "text-sky-600 dark:text-sky-400"
  in
  let neutral_checked = if st.is_neutral then " checked" else "" in
  let spread_class =
    if abs_margin < 1.5 then "bg-rose-100 text-rose-700 dark:bg-rose-900/30 dark:text-rose-300"
    else if abs_margin < 5.5 then "bg-amber-100 text-amber-700 dark:bg-amber-900/30 dark:text-amber-300"
    else "bg-emerald-100 text-emerald-700 dark:bg-emerald-900/30 dark:text-emerald-300"
  in
  let html = Printf.sprintf
    {html|<div class="space-y-3">
  <div class="flex items-center justify-between text-[10px] font-black uppercase tracking-widest text-slate-500">
    <span class="flex items-center gap-1.5"><span class="w-2 h-2 rounded-full bg-orange-500"></span>%s</span>
    <span class="flex items-center gap-1.5">%s<span class="w-2 h-2 rounded-full bg-sky-500"></span></span>
  </div>
  <div class="relative h-5 bg-slate-100 dark:bg-slate-800 rounded-full overflow-hidden shadow-inner border border-slate-200/50 dark:border-slate-700/30">
    <div class="absolute top-0 left-1/2 -ml-px w-0.5 h-full bg-slate-300 dark:bg-slate-600 z-10 opacity-30"></div>
    <div class="flex h-full w-full">
      <div class="gauge-bar h-full bg-gradient-to-r from-orange-600 to-orange-400 shadow-[0_0_15px_rgba(249,115,22,0.3)]" style="width: 0%%" data-target="%.1f"></div>
      <div class="gauge-bar h-full bg-gradient-to-l from-sky-600 to-sky-400 shadow-[0_0_15px_rgba(14,165,233,0.3)]" style="width: 0%%" data-target="%.1f"></div>
    </div>
  </div>
  <div class="flex justify-between font-mono">
    <div class="flex flex-col">
      <span class="text-2xl font-black text-orange-700 dark:text-orange-400 leading-none gauge-pct" data-target="%.1f">0.0%%</span>
      <span class="text-[9px] text-slate-400 font-bold uppercase mt-1">%s</span>
    </div>
    <div class="flex flex-col items-center">
      <span class="text-sm font-black %s">%s</span>
      <span class="px-1.5 py-0.5 rounded text-[10px] font-bold %s">%s +%.1f</span>
    </div>
    <div class="flex flex-col items-end">
      <span class="text-2xl font-black text-sky-600 dark:text-sky-400 leading-none gauge-pct" data-target="%.1f">0.0%%</span>
      <span class="text-[9px] text-slate-400 font-bold uppercase mt-1">%s</span>
    </div>
  </div>
  <div class="grid grid-cols-3 gap-2 text-[10px] font-mono text-slate-500 dark:text-slate-400">
    <div class="text-center"><span class="block text-slate-700 dark:text-slate-300 font-bold">%.1f%% / %.1f%%</span>Elo</div>
    <div class="text-center"><span class="block text-slate-700 dark:text-slate-300 font-bold">%.1f%% / %.1f%%</span>Pythag</div>
    <div class="text-center"><span class="block text-slate-700 dark:text-slate-300 font-bold">%.1f%% / %.1f%%</span>Stats</div>
  </div>
  <div class="flex items-center justify-center gap-2 pt-1">
    <label class="flex items-center gap-2 cursor-pointer text-xs text-slate-500 dark:text-slate-400">
      <input type="checkbox" class="gauge-neutral-toggle accent-slate-600"%s>
      <span class="select-none font-mono">%s</span>
    </label>
  </div>
</div>|html}
    st.home_name
    st.away_name
    home_pct away_pct
    home_pct
    "\xEC\x8A\xB9\xEB\xA6\xAC \xED\x99\x95\xEB\xA5\xA0" (* 승리 확률 *)
    winner_cls winner
    spread_class badge abs_margin
    away_pct
    "\xEC\x8A\xB9\xEB\xA6\xAC \xED\x99\x95\xEB\xA5\xA0" (* 승리 확률 *)
    (elo_prob *. 100.0) ((1.0 -. elo_prob) *. 100.0)
    (pyth_prob *. 100.0) ((1.0 -. pyth_prob) *. 100.0)
    (stats_prob *. 100.0) ((1.0 -. stats_prob) *. 100.0)
    neutral_checked
    (if st.is_neutral
     then "\xEC\xA4\x91\xEB\xA6\xBD \xEA\xB2\xBD\xEA\xB8\xB0"
     else "\xED\x99\x88 \xEC\xBD\x94\xED\x8A\xB8 \xEC\xA0\x81\xEC\x9A\xA9")
  in
  set_inner_html container html

(* ── Animate gauge bars ────────────────────── *)

let animate_gauge (container : Dom_html.element Js.t) =
  (* Animate bars to target width *)
  let bars = query_all (container :> Dom.node Js.t) ".gauge-bar" in
  List.iter (fun bar_node ->
    let bar = to_element bar_node in
    let target = get_float bar "data-target" ~default:50.0 in
    bar##.style##.cssText := Js.string
      (Printf.sprintf "width: %.1f%%; transition: width 1.2s cubic-bezier(0.4, 0, 0.2, 1)" target)
  ) bars;
  (* Animate percentage text *)
  let pcts = query_all (container :> Dom.node Js.t) ".gauge-pct" in
  List.iter (fun pct_node ->
    let pct = to_element pct_node in
    let target = get_float pct "data-target" ~default:50.0 in
    pct##.textContent := Js.some (Js.string (Printf.sprintf "%.1f%%" target))
  ) pcts

(* ── Initialize a single gauge ─────────────── *)

let init_gauge (container : Dom_html.element Js.t) =
  let home_name = match get_attr container "data-home-name" with Some s -> s | None -> "Home" in
  let away_name = match get_attr container "data-away-name" with Some s -> s | None -> "Away" in
  let home_elo = get_float container "data-home-elo" ~default:1500.0 in
  let away_elo = get_float container "data-away-elo" ~default:1500.0 in
  let pyth_prob = get_float container "data-pyth-prob" ~default:0.5 in
  let stats_prob = get_float container "data-stats-prob" ~default:0.5 in
  let h2h_prob = get_float container "data-h2h-prob" ~default:0.5 in
  let is_neutral =
    match get_attr container "data-is-neutral" with
    | Some "true" -> true
    | _ -> false
  in
  (* Derive relative_strength from stats_prob:
     stats_prob = clamp01(relative_strength + home_advantage)
     If home game: relative_strength = stats_prob - home_court_boost
     If neutral:   relative_strength = stats_prob *)
  let stats_relative =
    if is_neutral then stats_prob
    else stats_prob -. Pc.home_court_boost
  in
  let st = { home_name; away_name; home_elo; away_elo;
             pyth_prob; stats_relative; h2h_prob; is_neutral } in
  (* Render initial gauge *)
  render_gauge container st;
  (* Trigger animation after a frame *)
  ignore (Dom_html.window##requestAnimationFrame
    (Js.wrap_callback (fun _ts -> animate_gauge container)));
  (* Attach toggle handler *)
  let toggles = query_all (container :> Dom.node Js.t) ".gauge-neutral-toggle" in
  List.iter (fun toggle_node ->
    let toggle = to_element toggle_node in
    ignore (Dom_html.addEventListener toggle Dom_html.Event.change
      (Dom_html.handler (fun _ev ->
        let checked : bool = Js.to_bool (Js.Unsafe.get toggle "checked") in
        st.is_neutral <- checked;
        render_gauge container st;
        ignore (Dom_html.window##requestAnimationFrame
          (Js.wrap_callback (fun _ts -> animate_gauge container)));
        Js._true))
      Js._false)
  ) toggles

(* ── Hydration entry point ─────────────────── *)

let hydrate_subtree (root : Dom.node Js.t) =
  let containers = query_all root "[data-island='prediction_gauge']:not([data-island-hydrated])" in
  List.iter (fun container_node ->
    let container = to_element container_node in
    init_gauge container;
    container##setAttribute (Js.string "data-island-hydrated") (Js.string "true")
  ) containers

let () =
  Console.console##log
    (Js.string "[wasm-island:prediction_gauge] loaded");
  hydrate_subtree (Dom_html.document :> Dom.node Js.t);
  ignore (Dom_html.addEventListener Dom_html.document##.body
    (Dom_html.Event.make "htmx:afterSwap")
    (Dom_html.handler (fun ev ->
      let target =
        Js.Opt.get (Js.Unsafe.get (Js.Unsafe.get ev "detail") "target")
          (fun () -> (Dom_html.document :> Dom.node Js.t)) in
      hydrate_subtree target;
      Js._true))
    Js._false)
