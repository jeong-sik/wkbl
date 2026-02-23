(** Shared UI components built on Html_dsl.

    These are reusable visual building blocks extracted from views modules.
    Each function returns [Html_dsl.html] — compose freely with other DSL nodes. *)

open Html_dsl

(* ── Empty state ──────────────────────────────────── *)

type empty_state_icon =
  | BasketballIcon
  | SearchIcon
  | ChartIcon
  | UsersIcon
  | TableIcon

let icon_svg = function
  | BasketballIcon -> raw {|<span class="text-4xl">🏀</span>|}
  | SearchIcon ->
      raw {|<svg class="w-10 h-10 text-slate-400" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"/></svg>|}
  | ChartIcon ->
      raw {|<svg class="w-10 h-10 text-slate-400" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"/></svg>|}
  | UsersIcon ->
      raw {|<svg class="w-10 h-10 text-slate-400" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4.354a4 4 0 110 5.292M15 21H3v-1a6 6 0 0112 0v1zm0 0h6v-1a6 6 0 00-9-5.197M13 7a4 4 0 11-8 0 4 4 0 018 0z"/></svg>|}
  | TableIcon ->
      raw {|<svg class="w-10 h-10 text-slate-400" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 10h18M3 14h18m-9-4v8m-7 0h14a2 2 0 002-2V8a2 2 0 00-2-2H5a2 2 0 00-2 2v8a2 2 0 002 2z"/></svg>|}

(** Empty state placeholder with icon, title, and description. *)
let empty_state ?(icon = BasketballIcon) title_text desc_text =
  div [cls "text-center py-12 px-4"] [
    div [cls "mb-4 flex justify-center"] [icon_svg icon];
    h3 [cls "text-lg font-bold text-slate-900 dark:text-slate-200"] [text title_text];
    p [cls "text-slate-500 dark:text-slate-400"] [text desc_text];
  ]

(* ── Efficiency badge ─────────────────────────────── *)

(** EFF badge with color threshold at 20.0. *)
let eff_badge ?(show_label = false) eff =
  let color_cls =
    if eff >= 20.0 then
      "bg-orange-500/10 text-orange-700 dark:text-orange-400 border-orange-500/30"
    else
      "bg-slate-100 dark:bg-slate-800 text-slate-600 dark:text-slate-400 border-slate-300 dark:border-slate-700"
  in
  span [cls ("px-2 py-0.5 rounded border text-[10px] font-mono font-bold " ^ color_cls)] [
    when_ show_label (text "EFF ");
    float_ eff;
  ]

(* ── Score quality badge ──────────────────────────── *)

type score_quality = Verified | Derived | Mismatch

(** Score quality indicator badge. *)
let score_quality_badge ?(compact = false) quality label_text =
  let pad_cls = if compact then "px-1.5" else "px-2" in
  let color_cls = match quality with
    | Verified  -> "bg-sky-500/10 text-sky-600 dark:text-sky-400 border-sky-500/30"
    | Derived   -> "bg-amber-500/10 text-amber-600 dark:text-amber-400 border-amber-500/30"
    | Mismatch  -> "bg-rose-500/10 text-rose-600 dark:text-rose-400 border-rose-500/30"
  in
  span [cls (pad_cls ^ " py-0.5 rounded border text-[10px] font-bold whitespace-nowrap " ^ color_cls)] [
    text label_text;
  ]

(* ── Stat card ────────────────────────────────────── *)

(** Compact stat card for dashboards (label + value). *)
let stat_card ?(extra_cls = "") label_text value_html =
  div [cls ("bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-lg p-4 " ^ extra_cls)] [
    div [cls "text-xs text-slate-500 dark:text-slate-400 mb-1"] [text label_text];
    div [cls "text-xl font-bold font-mono text-slate-900 dark:text-white"] [value_html];
  ]

(** Stat card with numeric value. *)
let stat_card_int ?(extra_cls = "") label_text n =
  stat_card ~extra_cls label_text (int n)

(** Stat card with float value. *)
let stat_card_float ?(extra_cls = "") ?(decimals = 1) label_text f =
  stat_card ~extra_cls label_text (float_ ~decimals f)

(* ── Section header ───────────────────────────────── *)

(** Section header with title and optional action slot. *)
let section_header ?action_html title_text =
  div [cls "flex items-center justify-between mb-4"] [
    h2 [cls "text-lg font-bold text-slate-900 dark:text-white"] [text title_text];
    maybe (fun a -> a) action_html;
  ]

(* ── Loading spinner ──────────────────────────────── *)

(** HTMX loading indicator. *)
let loading_spinner ?(sr_text = "Loading...") () =
  span [cls "htmx-indicator"] [
    span [
      cls "w-4 h-4 border-2 border-slate-300 border-t-orange-500 rounded-full animate-spin inline-block";
      aria "hidden" "true";
    ] [];
    span [cls "sr-only"] [text sr_text];
  ]
