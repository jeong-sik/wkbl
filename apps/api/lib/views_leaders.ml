open Domain
open Views_common
open Html_dsl

(* ── Podium leader card ───────────────────────── *)

(** Build a disambiguation line for duplicate player names. *)
let disambiguation_html show_id (l: leader_entry) info_opt =
  if show_id l then
    raw (player_disambiguation_line ~team_name:l.le_team_name ~player_id:l.le_player_id info_opt)
  else empty

(** Podium item: rank 1=gold/large, 2=silver/medium, 3=bronze/medium. *)
let podium_item ~show_id ~info_opt ~value_fmt rank (l: leader_entry) =
  let bg, ring, size, mt, hover_scale = match rank with
    | 1 -> ("bg-amber-400/20", "ring-amber-400", "w-14 h-14", "mt-0", "hover:scale-110")
    | 2 -> ("bg-slate-300/20", "ring-slate-400", "w-11 h-11", "mt-4", "hover:scale-105")
    | _ -> ("bg-amber-700/20", "ring-amber-700", "w-11 h-11", "mt-6", "hover:scale-105")
  in
  a [href (player_href l.le_player_id); cls (Printf.sprintf "flex flex-col items-center %s group cursor-pointer transition-transform duration-200 %s" mt hover_scale)] [
    div [cls "relative"] [
      div [cls (Printf.sprintf "%s rounded-full %s ring-2 overflow-hidden group-hover:ring-4 transition-all duration-200" size bg)] [
        raw (player_img_tag ~class_name:(size ^ " object-cover") l.le_player_id l.le_player_name);
      ];
      div [cls (Printf.sprintf "absolute -bottom-1 left-1/2 -translate-x-1/2 %s text-xs font-bold w-5 h-5 rounded-full flex items-center justify-center text-white group-hover:scale-110 transition-transform" ring)] [
        int rank;
      ];
    ];
    div [cls "text-lg font-bold text-slate-900 dark:text-slate-200 mt-2 group-hover:text-orange-500 transition-colors"] [
      text (escape_html (value_fmt l.le_stat_value));
    ];
    span [cls "text-xs text-slate-600 dark:text-slate-400 truncate max-w-[80px]"] [
      text (escape_html (normalize_name l.le_player_name));
    ];
    disambiguation_html show_id l (info_opt l.le_player_id);
  ]

(** 4th+ compact list row. *)
let others_row ~show_id ~info_opt ~value_fmt i (l: leader_entry) =
  a [href (player_href l.le_player_id); cls "flex items-center justify-between py-1.5 border-b border-slate-200/50 dark:border-slate-800/40 last:border-0 hover:bg-slate-100 dark:hover:bg-slate-800/50 -mx-2 px-2 rounded transition-colors group"] [
    div [cls "flex items-center gap-2 min-w-0"] [
      span [cls "text-slate-500 dark:text-slate-400 font-mono text-xs w-4 group-hover:text-orange-500 transition-colors"] [
        int (i + 4);
      ];
      raw (player_img_tag ~class_name:"w-6 h-6" l.le_player_id l.le_player_name);
      div [cls "flex flex-col min-w-0"] [
        span [cls "text-xs text-slate-700 dark:text-slate-300 group-hover:text-orange-500 transition-colors truncate"] [
          text (escape_html (normalize_name l.le_player_name));
        ];
        disambiguation_html show_id l (info_opt l.le_player_id);
      ];
    ];
    span [cls "font-mono text-xs text-slate-600 dark:text-slate-400 group-hover:font-bold transition-all"] [
      text (escape_html (value_fmt l.le_stat_value));
    ];
  ]

let leader_card ?(player_info_map=None) ?(value_fmt=(fun v -> Printf.sprintf "%.1f" v)) title (leaders: leader_entry list) =
  if leaders = [] then ""
  else
    let name_counts : (string, int) Hashtbl.t = Hashtbl.create 16 in
    leaders |> List.iter (fun (l: leader_entry) ->
      let key = normalize_name l.le_player_name in
      Hashtbl.replace name_counts key ((Hashtbl.find_opt name_counts key |> Option.value ~default:0) + 1));
    let show_id (l: leader_entry) =
      Hashtbl.find_opt name_counts (normalize_name l.le_player_name)
      |> Option.map (fun c -> c > 1) |> Option.value ~default:false
    in
    let info_opt player_id =
      match player_info_map with
      | Some map -> Hashtbl.find_opt map player_id
      | None -> None
    in
    let top3, rest = match leaders with
      | a :: b :: c :: tl -> ([a; b; c], tl)
      | a :: b :: tl -> ([a; b], tl)
      | a :: tl -> ([a], tl)
      | [] -> ([], [])
    in
    let podium = podium_item ~show_id ~info_opt ~value_fmt in
    let podium_html = match top3 with
      | [first] ->
        div [cls "flex justify-center mb-4"] [podium 1 first]
      | [first; second] ->
        div [cls "flex justify-center items-end gap-4 mb-4"] [podium 2 second; podium 1 first]
      | [first; second; third] ->
        div [cls "flex justify-center items-end gap-3 mb-4"] [podium 2 second; podium 1 first; podium 3 third]
      | _ -> empty
    in
    let others_html = rest |> List.mapi (others_row ~show_id ~info_opt ~value_fmt) in
    to_string (
      div [cls "bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg hover:shadow-xl hover:border-orange-300 dark:hover:border-orange-700 transition-all duration-300 card-enter"] [
        h3 [cls "text-slate-500 dark:text-slate-400 font-bold uppercase tracking-wider text-[10px] mb-3 text-center"] [
          text (escape_html title);
        ];
        podium_html;
        div [cls "space-y-0"] others_html;
      ]
    )

(** Signed value format for +/- stats (reuses podium style) *)
let leader_card_signed ?(player_info_map=None) title leaders =
  let signed v = if v > 0.0 then Printf.sprintf "+%.1f" v else Printf.sprintf "%.1f" v in
  leader_card ~player_info_map ~value_fmt:signed title leaders

(* ── Season option builder ────────────────────── *)

let season_option_list ~season (seasons: season_info list) =
  let base = seasons |> List.map (fun (s: season_info) ->
    option [value s.code; if s.code = season then selected else cls ""] [text (escape_html s.name)]
  ) in
  concat (
    option [value "ALL"; if season = "ALL" then selected else cls ""] [text "전체 시즌"]
    :: base
  )

(* ── Leaders page ─────────────────────────────── *)

let leaders_page ?(lang=I18n.Ko) ?(player_info_map=None) ~season ~seasons ~scope (leaders_by_category: (string * leader_entry list) list) =
  let scope_value = scope |> String.trim |> String.lowercase_ascii in
  let lookup category =
    leaders_by_category
    |> List.find_opt (fun (k, _) -> k = category)
    |> Option.map snd
    |> Option.value ~default:[]
  in
  let fmt_int v = Printf.sprintf "%.0f" v in
  let fmt_f1 v = Printf.sprintf "%.1f" v in
  let fmt_f3 v = Printf.sprintf "%.3f" v in
  let main_categories, shooting_categories =
    match scope_value with
    | "totals" ->
      ( [ ("경기", "gp", fmt_int); ("출전시간", "min", fmt_f1); ("득점", "pts", fmt_int)
        ; ("리바운드", "reb", fmt_int); ("어시스트", "ast", fmt_int); ("스틸", "stl", fmt_int)
        ; ("블록", "blk", fmt_int); ("턴오버", "tov", fmt_int) ]
      , [ ("FG%", "fg_pct", fmt_f3); ("3P%", "fg3_pct", fmt_f3); ("FT%", "ft_pct", fmt_f3)
        ; ("TS%", "ts_pct", fmt_f3); ("eFG%", "efg_pct", fmt_f3); ("USG%", "usg_pct", fmt_f1) ] )
    | "per_36" ->
      ( [ ("득점/36", "pts", fmt_f1); ("리바운드/36", "reb", fmt_f1); ("어시스트/36", "ast", fmt_f1)
        ; ("스틸/36", "stl", fmt_f1); ("블록/36", "blk", fmt_f1); ("턴오버/36", "tov", fmt_f1)
        ; ("EFF/36", "eff", fmt_f1) ]
      , [ ("FG%", "fg_pct", fmt_f3); ("3P%", "fg3_pct", fmt_f3); ("FT%", "ft_pct", fmt_f3)
        ; ("TS%", "ts_pct", fmt_f3); ("eFG%", "efg_pct", fmt_f3); ("USG%", "usg_pct", fmt_f1) ] )
    | _ ->
      ( [ ("득점", "pts", fmt_f1); ("리바운드", "reb", fmt_f1); ("어시스트", "ast", fmt_f1)
        ; ("스틸", "stl", fmt_f1); ("블록", "blk", fmt_f1); ("턴오버", "tov", fmt_f1)
        ; ("출전시간", "min", fmt_f1); ("EFF", "eff", fmt_f1) ]
      , [ ("FG%", "fg_pct", fmt_f3); ("3P%", "fg3_pct", fmt_f3); ("FT%", "ft_pct", fmt_f3)
        ; ("TS%", "ts_pct", fmt_f3); ("eFG%", "efg_pct", fmt_f3); ("USG%", "usg_pct", fmt_f1) ] )
  in
  let render_cards categories =
    categories
    |> List.map (fun (title, key, fmt) ->
      raw (leader_card ~player_info_map ~value_fmt:fmt title (lookup key)))
  in
  let scope_opt v label =
    option [value v; if scope_value = v then selected else cls ""] [text label]
  in
  let content = to_string (
    div [cls "space-y-8"] [
      div [cls "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-6"] (render_cards main_categories);
      raw {html|<div class="text-[11px] text-slate-600 dark:text-slate-400 leading-relaxed">슈팅 부문은 최소 시도 기준이 있습니다: <span class="font-mono text-slate-700 dark:text-slate-300">FG≥50</span>, <span class="font-mono text-slate-700 dark:text-slate-300">3P≥20</span>, <span class="font-mono text-slate-700 dark:text-slate-300">FT≥20</span>. Per-36 부문은 <span class="font-mono text-slate-700 dark:text-slate-300">MIN≥100</span> 기준입니다.</div>|html};
      div [cls "flex items-baseline justify-between"] [
        h3 [cls "text-lg font-bold text-slate-900 dark:text-slate-200"] [text "슈팅"];
        div [cls "text-xs text-slate-600 dark:text-slate-400"] [text "FG / 3P / FT / TS / eFG"];
      ];
      div [cls "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-6"] (render_cards shooting_categories);
    ]
  ) in
  layout
    ~lang
    ~title:"WKBL 리더"
    ~canonical_path:"/leaders"
    ~description:"WKBL 여자농구 리더보드 - 득점, 리바운드, 어시스트, 슛 퍼센트 등 부문별 선두 선수 순위"
    ~content:(to_string (
      div [cls "space-y-8 animate-fade-in"] [
        raw (breadcrumb [("홈", "/"); ("리더", "")]);
        div [cls "flex flex-col md:flex-row md:items-center md:justify-between gap-3"] [
          div [] [
            h2 [cls "text-3xl font-black text-slate-900 dark:text-slate-200"] [text "부문별 1위"];
            p [cls "text-slate-600 dark:text-slate-400"] [text "부문별 선두 선수 기록입니다."];
          ];
          form [action "/leaders"; method_ "get"; cls "grid grid-cols-1 sm:grid-cols-2 gap-3 w-full md:w-auto"] [
            select [name "season"; aria "label" "시즌 선택"; cls "bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-full sm:w-48"; data "auto-submit" "change"] [
              season_option_list ~season seasons;
            ];
            select [name "scope"; aria "label" "기준 선택"; cls "bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-full sm:w-48"; data "auto-submit" "change"] [
              scope_opt "per_game" "경기당";
              scope_opt "totals" "누적";
              scope_opt "per_36" "36분 환산";
            ];
          ];
        ];
        raw content;
      ]
    )) ()

(* ── Clutch page ──────────────────────────────── *)

let clutch_page ?(lang=I18n.Ko) ~season ~seasons (stats: clutch_stats list) =
  let t = I18n.t lang in
  let all_seasons_label = t { ko = "전체 시즌"; en = "All seasons" } in
  let h2_title = t { ko = "클러치 리더"; en = "Clutch Leaders" } in
  let subtitle = t { ko = "4쿼터 5분 이하 + 점수차 5점 이하"; en = "Q4 last 5 minutes and score diff within 5" } in
  let empty_title = t { ko = "클러치 기록이 없습니다"; en = "No clutch stats" } in
  let empty_desc = t { ko = "클러치 = 4쿼터 5분 이하 + 점수차 5점 이하"; en = "Clutch = Q4 last 5 minutes and score diff within 5" } in
  let footnote = t { ko = "문자중계 기록을 기반으로 집계합니다."; en = "Aggregated from play-by-play records." } in
  let th_player = t { ko = "선수"; en = "Player" } in
  let th_team = t { ko = "팀"; en = "Team" } in
  let season_opts =
    let base = seasons |> List.map (fun (s: season_info) ->
      option [value s.code; if s.code = season then selected else cls ""] [text (escape_html s.name)]
    ) in
    concat (
      option [value "ALL"; if season = "ALL" then selected else cls ""] [text (escape_html all_seasons_label)]
      :: base
    )
  in
  let clutch_rows = stats |> List.map (fun (s: clutch_stats) ->
    let fg_pct_str = if s.cs_clutch_fg_att > 0
      then Printf.sprintf "%.1f%%" (s.cs_clutch_fg_pct *. 100.0) else "-" in
    tr [cls "border-b border-slate-200 dark:border-slate-700 hover:bg-slate-50 dark:hover:bg-slate-800/50 text-sm font-mono tabular-nums"] [
      td [cls "px-3 py-2 font-sans"] [
        a [href (player_href s.cs_player_id); cls "text-orange-500 hover:underline font-medium"] [text (escape_html s.cs_player_name)];
      ];
      td [cls "px-3 py-2 text-slate-600 dark:text-slate-400 font-sans"] [text (escape_html s.cs_team_name)];
      td [cls "px-3 py-2 text-right w-16"] [int s.cs_clutch_games];
      td [cls "px-3 py-2 text-right font-bold text-slate-900 dark:text-slate-200 w-16"] [int s.cs_clutch_points];
      td [cls "px-3 py-2 text-right text-slate-600 dark:text-slate-400 w-24"] [
        text (Printf.sprintf "%d-%d" s.cs_clutch_fg_made s.cs_clutch_fg_att);
      ];
      td [cls "px-3 py-2 text-right text-slate-600 dark:text-slate-400 w-20"] [text fg_pct_str];
      td [cls "px-3 py-2 text-right text-slate-600 dark:text-slate-400 w-16"] [int s.cs_clutch_3p_made];
      td [cls "px-3 py-2 text-right text-slate-600 dark:text-slate-400 w-24"] [
        text (Printf.sprintf "%d-%d" s.cs_clutch_ft_made s.cs_clutch_ft_att);
      ];
    ]
  ) in
  let empty_row =
    if stats = [] then
      el "tr" [] [
        td [attr "colspan" "8"] [
          Views_components.empty_state ~icon:BasketballIcon (escape_html empty_title) (escape_html empty_desc);
        ];
      ]
    else empty
  in
  let col w = void_el "col" [style ("width: " ^ w ^ ";")] in
  let table_html =
    div [cls "overflow-x-auto"] [
      table [cls "w-full text-sm table-fixed font-mono tabular-nums"] [
        el "colgroup" [] [
          col "auto"; col "140px"; col "60px"; col "60px";
          col "100px"; col "80px"; col "60px"; col "100px";
        ];
        thead [cls "bg-slate-100 dark:bg-slate-800 text-slate-700 dark:text-slate-300"] [
          el "tr" [] [
            th [attr "scope" "col"; cls "px-3 py-2 text-left font-sans"] [text (escape_html th_player)];
            th [attr "scope" "col"; cls "px-3 py-2 text-left font-sans"] [text (escape_html th_team)];
            th [attr "scope" "col"; cls "px-3 py-2 text-center font-sans"; title "클러치 경기"] [text "GP"];
            th [attr "scope" "col"; cls "px-3 py-2 text-center font-sans"; title "클러치 득점"] [text "PTS"];
            th [attr "scope" "col"; cls "px-3 py-2 text-center font-sans"; title "야투 성공-시도"] [text "FGM-A"];
            th [attr "scope" "col"; cls "px-3 py-2 text-center font-sans"; title "야투 성공률"] [text "FG%"];
            th [attr "scope" "col"; cls "px-3 py-2 text-center font-sans"; title "3점 성공"] [text "3PM"];
            th [attr "scope" "col"; cls "px-3 py-2 text-center font-sans"; title "자유투 성공-시도"] [text "FTM-A"];
          ];
        ];
        tbody [] (clutch_rows @ [empty_row]);
      ];
    ]
  in
  layout
    ~lang
    ~title:(t { ko = "클러치 리더 | WKBL"; en = "Clutch Leaders | WKBL" })
    ~canonical_path:"/clutch"
    ~description:(t { ko = "WKBL 여자농구 클러치 타임 리더 - 접전 상황(4쿼터 5분 이내) 득점 기록."; en = "Clutch leaders in close games (Q4 last 5 minutes)." })
    ~content:(to_string (
      div [cls "space-y-6 animate-fade-in"] [
        raw (breadcrumb [("홈", "/"); ("클러치 리더", "")]);
        div [cls "flex flex-col md:flex-row md:items-center md:justify-between gap-3"] [
          div [] [
            h2 [cls "text-3xl font-black text-slate-900 dark:text-slate-200"] [text (escape_html h2_title)];
            p [cls "text-slate-600 dark:text-slate-400"] [text (escape_html subtitle)];
          ];
          form [action "/clutch"; method_ "get"; cls "flex items-center gap-3"] [
            select [name "season"; aria "label" "시즌 선택"; cls "bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-48"; data "auto-submit" "change"] [
              season_opts;
            ];
          ];
        ];
        div [cls "bg-white dark:bg-slate-900 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700"] [
          table_html;
        ];
        div [cls "text-xs text-slate-600 dark:text-slate-400 text-center"] [text (escape_html footnote)];
      ]
    )) ()
