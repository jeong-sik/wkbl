(** Share Card Generator - SVG templates for social media sharing

    Generates preview cards for:
    - Player stats
    - Game results
    - Player comparisons
*)

open Domain

(** Card dimensions (1200x630 - Twitter/OG standard) *)
let card_width = 1200
let card_height = 630

(** Team colors for background gradients - match by team name substring *)
let team_gradient team_name =
  if String.length team_name >= 2 then
    let name = String.lowercase_ascii team_name in
    if String.sub name 0 2 = "삼성" || String.sub name 0 4 = "samsung" then ("#1e3a8a", "#3b82f6")  (* 삼성생명 - 블루 *)
    else if String.sub name 0 2 = "우리" || String.sub name 0 5 = "woori" then ("#7f1d1d", "#ef4444")  (* 우리은행 - 레드 *)
    else if String.sub name 0 2 = "kb" || String.sub name 0 2 = "KB" then ("#78350f", "#f59e0b")  (* KB스타즈 - 옐로우 *)
    else if String.sub name 0 2 = "신한" || String.sub name 0 7 = "shinhan" then ("#14532d", "#22c55e")  (* 신한은행 - 그린 *)
    else if String.sub name 0 3 = "bnk" || String.sub name 0 3 = "BNK" then ("#4c1d95", "#8b5cf6")  (* BNK썸 - 퍼플 *)
    else if String.sub name 0 2 = "하나" || String.sub name 0 4 = "hana" then ("#0c4a6e", "#0ea5e9")  (* 하나원큐 - 스카이블루 *)
    else ("#1e293b", "#475569")  (* 기본 - 슬레이트 *)
  else ("#1e293b", "#475569")

(** Escape XML characters *)
let escape_xml s =
  s
  |> String.to_seq
  |> Seq.map (function
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '&' -> "&amp;"
    | '"' -> "&quot;"
    | '\'' -> "&apos;"
    | c -> String.make 1 c)
  |> List.of_seq
  |> String.concat ""

(** Generate player card SVG *)
let player_card (p: player_aggregate) =
  let (grad_start, grad_end) = team_gradient p.team_name in
  let efficiency_color =
    if p.efficiency >= 20.0 then "#22c55e"  (* 그린 - MVP급 *)
    else if p.efficiency >= 15.0 then "#3b82f6"  (* 블루 - 우수 *)
    else if p.efficiency >= 10.0 then "#f59e0b"  (* 옐로우 - 양호 *)
    else "#94a3b8"  (* 슬레이트 - 보통 *)
  in
  Printf.sprintf {|<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">
  <defs>
    <linearGradient id="bgGrad" x1="0%%" y1="0%%" x2="100%%" y2="100%%">
      <stop offset="0%%" style="stop-color:%s"/>
      <stop offset="100%%" style="stop-color:%s"/>
    </linearGradient>
    <filter id="shadow" x="-20%%" y="-20%%" width="140%%" height="140%%">
      <feDropShadow dx="0" dy="4" stdDeviation="8" flood-opacity="0.3"/>
    </filter>
  </defs>

  <!-- Background -->
  <rect width="%d" height="%d" fill="url(#bgGrad)"/>

  <!-- Dark overlay for readability -->
  <rect width="%d" height="%d" fill="#0f172a" opacity="0.7"/>

  <!-- Content container -->
  <g transform="translate(60, 60)">
    <!-- WKBL Branding -->
    <text x="0" y="40" font-family="system-ui, sans-serif" font-size="24" font-weight="600" fill="#f97316">
      🏀 WKBL Analytics
    </text>

    <!-- Player Name -->
    <text x="0" y="140" font-family="system-ui, sans-serif" font-size="72" font-weight="800" fill="#ffffff">
      %s
    </text>

    <!-- Team Name -->
    <text x="0" y="200" font-family="system-ui, sans-serif" font-size="32" font-weight="500" fill="#94a3b8">
      %s
    </text>

    <!-- Stats Grid -->
    <g transform="translate(0, 280)">
      <!-- EFF (highlighted) -->
      <g transform="translate(0, 0)">
        <rect x="-10" y="-50" width="200" height="120" rx="12" fill="%s" opacity="0.2"/>
        <text x="80" y="0" font-family="system-ui, sans-serif" font-size="64" font-weight="800" fill="%s" text-anchor="middle">
          %.1f
        </text>
        <text x="80" y="40" font-family="system-ui, sans-serif" font-size="20" fill="#94a3b8" text-anchor="middle">
          EFF
        </text>
      </g>

      <!-- PTS -->
      <g transform="translate(240, 0)">
        <text x="60" y="0" font-family="system-ui, sans-serif" font-size="48" font-weight="700" fill="#ffffff" text-anchor="middle">
          %.1f
        </text>
        <text x="60" y="40" font-family="system-ui, sans-serif" font-size="18" fill="#94a3b8" text-anchor="middle">
          PTS
        </text>
      </g>

      <!-- REB -->
      <g transform="translate(400, 0)">
        <text x="60" y="0" font-family="system-ui, sans-serif" font-size="48" font-weight="700" fill="#ffffff" text-anchor="middle">
          %.1f
        </text>
        <text x="60" y="40" font-family="system-ui, sans-serif" font-size="18" fill="#94a3b8" text-anchor="middle">
          REB
        </text>
      </g>

      <!-- AST -->
      <g transform="translate(560, 0)">
        <text x="60" y="0" font-family="system-ui, sans-serif" font-size="48" font-weight="700" fill="#ffffff" text-anchor="middle">
          %.1f
        </text>
        <text x="60" y="40" font-family="system-ui, sans-serif" font-size="18" fill="#94a3b8" text-anchor="middle">
          AST
        </text>
      </g>

      <!-- GP -->
      <g transform="translate(720, 0)">
        <text x="60" y="0" font-family="system-ui, sans-serif" font-size="48" font-weight="700" fill="#ffffff" text-anchor="middle">
          %d
        </text>
        <text x="60" y="40" font-family="system-ui, sans-serif" font-size="18" fill="#94a3b8" text-anchor="middle">
          GP
        </text>
      </g>
    </g>

    <!-- Footer -->
    <text x="0" y="480" font-family="system-ui, sans-serif" font-size="18" fill="#64748b">
      wkbl.win
    </text>
  </g>

  <!-- Decorative accent -->
  <rect x="0" y="%d" width="%d" height="8" fill="#f97316"/>
</svg>|}
    card_width card_height card_width card_height
    grad_start grad_end
    card_width card_height
    card_width card_height
    (escape_xml p.name)
    (escape_xml p.team_name)
    efficiency_color efficiency_color p.efficiency
    p.avg_points
    p.avg_rebounds
    p.avg_assists
    p.games_played
    (card_height - 8) card_width

(** Generate game result card SVG *)
let game_card (home_team: string) (home_score: int) (away_team: string) (away_score: int) (game_date: string) =
  let winner_highlight = if home_score > away_score then "#22c55e" else if away_score > home_score then "#ef4444" else "#f97316" in
  Printf.sprintf {|<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">
  <defs>
    <linearGradient id="bgGrad" x1="0%%" y1="0%%" x2="100%%" y2="100%%">
      <stop offset="0%%" style="stop-color:#0f172a"/>
      <stop offset="100%%" style="stop-color:#1e293b"/>
    </linearGradient>
  </defs>

  <!-- Background -->
  <rect width="%d" height="%d" fill="url(#bgGrad)"/>

  <!-- Content -->
  <g transform="translate(60, 60)">
    <!-- WKBL Branding -->
    <text x="0" y="40" font-family="system-ui, sans-serif" font-size="24" font-weight="600" fill="#f97316">
      🏀 WKBL Analytics
    </text>

    <!-- Game Date -->
    <text x="0" y="100" font-family="system-ui, sans-serif" font-size="24" fill="#94a3b8">
      %s
    </text>

    <!-- Score Display -->
    <g transform="translate(0, 200)">
      <!-- Home Team -->
      <text x="300" y="0" font-family="system-ui, sans-serif" font-size="48" font-weight="700" fill="#ffffff" text-anchor="end">
        %s
      </text>
      <text x="400" y="0" font-family="system-ui, sans-serif" font-size="72" font-weight="800" fill="%s" text-anchor="middle">
        %d
      </text>

      <!-- VS -->
      <text x="540" y="0" font-family="system-ui, sans-serif" font-size="32" fill="#64748b" text-anchor="middle">
        vs
      </text>

      <!-- Away Team -->
      <text x="680" y="0" font-family="system-ui, sans-serif" font-size="72" font-weight="800" fill="%s" text-anchor="middle">
        %d
      </text>
      <text x="780" y="0" font-family="system-ui, sans-serif" font-size="48" font-weight="700" fill="#ffffff" text-anchor="start">
        %s
      </text>
    </g>

    <!-- Footer -->
    <text x="0" y="380" font-family="system-ui, sans-serif" font-size="18" fill="#64748b">
      wkbl.win
    </text>
  </g>

  <!-- Winner accent -->
  <rect x="0" y="%d" width="%d" height="8" fill="%s"/>
</svg>|}
    card_width card_height card_width card_height
    card_width card_height
    (escape_xml game_date)
    (escape_xml home_team)
    (if home_score >= away_score then "#22c55e" else "#ffffff")
    home_score
    (if away_score >= home_score then "#ef4444" else "#ffffff")
    away_score
    (escape_xml away_team)
    (card_height - 8) card_width winner_highlight

(** Convert SVG to PNG using rsvg-convert *)
let svg_to_png svg_content =
  let tmp_svg = Filename.temp_file "wkbl_card_" ".svg" in
  let tmp_png = Filename.temp_file "wkbl_card_" ".png" in
  try
    (* Write SVG to temp file *)
    let oc = open_out tmp_svg in
    output_string oc svg_content;
    close_out oc;

    (* Convert to PNG using rsvg-convert *)
    let cmd = Printf.sprintf "rsvg-convert -o %s %s" (Filename.quote tmp_png) (Filename.quote tmp_svg) in
    let exit_code = Sys.command cmd in

    if exit_code = 0 then begin
      (* Read PNG file *)
      let ic = open_in_bin tmp_png in
      let n = in_channel_length ic in
      let data = really_input_string ic n in
      close_in ic;
      Sys.remove tmp_svg;
      Sys.remove tmp_png;
      Some data
    end else begin
      Sys.remove tmp_svg;
      (try Sys.remove tmp_png with _ -> ());
      None
    end
  with _ ->
    (try Sys.remove tmp_svg with _ -> ());
    (try Sys.remove tmp_png with _ -> ());
    None
