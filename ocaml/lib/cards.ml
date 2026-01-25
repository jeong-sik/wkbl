(** Share Card Generator for WKBL
    Generates SVG cards that can be converted to PNG for social sharing *)

(** Card dimensions - OG image standard (1200x630) *)
let card_width = 1200
let card_height = 630

(** Team colors for background gradient *)
let team_gradient team_name =
  match team_name with
  | "우리은행" -> ("#005BAA", "#003d7a")  (* Blue *)
  | "삼성생명" -> ("#007AFF", "#0055b3")  (* Blue *)
  | "신한은행" -> ("#2B3990", "#1a2463")  (* Navy *)
  | "KB스타즈" -> ("#FFCC00", "#cc9900")  (* Gold *)
  | "하나은행" -> ("#009490", "#006b68")  (* Teal *)
  | "BNK썸" -> ("#D6001C", "#9a0014")     (* Red *)
  | _ -> ("#1e293b", "#0f172a")           (* Default - Slate *)

(** Generate player card SVG from player_aggregate *)
let player_card (p : Domain.player_aggregate) =
  let (color1, color2) = team_gradient p.team_name in
  Printf.sprintf {|<?xml version="1.0" encoding="UTF-8"?>
<svg width="%d" height="%d" xmlns="http://www.w3.org/2000/svg">
  <defs>
    <linearGradient id="bg" x1="0%%" y1="0%%" x2="100%%" y2="100%%">
      <stop offset="0%%" style="stop-color:%s"/>
      <stop offset="100%%" style="stop-color:%s"/>
    </linearGradient>
  </defs>
  <rect width="%d" height="%d" fill="url(#bg)"/>
  <!-- Decorative circles -->
  <circle cx="1100" cy="100" r="300" fill="white" fill-opacity="0.05"/>
  <circle cx="-50" cy="580" r="200" fill="white" fill-opacity="0.05"/>
  <circle cx="200" cy="315" r="150" fill="white" fill-opacity="0.2"/>
  <!-- Player name and team -->
  <text x="100" y="120" font-family="sans-serif" font-size="72" font-weight="bold" fill="white">%s</text>
  <text x="100" y="180" font-family="sans-serif" font-size="36" fill="white" fill-opacity="0.8">%s</text>
  <!-- Stats boxes -->
  <g transform="translate(100, 280)">
    <rect width="200" height="120" rx="12" fill="white" fill-opacity="0.15"/>
    <text x="100" y="50" font-size="24" fill="white" fill-opacity="0.7" text-anchor="middle">득점</text>
    <text x="100" y="100" font-size="48" font-weight="bold" fill="white" text-anchor="middle">%.1f</text>
  </g>
  <g transform="translate(320, 280)">
    <rect width="200" height="120" rx="12" fill="white" fill-opacity="0.15"/>
    <text x="100" y="50" font-size="24" fill="white" fill-opacity="0.7" text-anchor="middle">리바운드</text>
    <text x="100" y="100" font-size="48" font-weight="bold" fill="white" text-anchor="middle">%.1f</text>
  </g>
  <g transform="translate(540, 280)">
    <rect width="200" height="120" rx="12" fill="white" fill-opacity="0.15"/>
    <text x="100" y="50" font-size="24" fill="white" fill-opacity="0.7" text-anchor="middle">어시스트</text>
    <text x="100" y="100" font-size="48" font-weight="bold" fill="white" text-anchor="middle">%.1f</text>
  </g>
  <g transform="translate(760, 280)">
    <rect width="200" height="120" rx="12" fill="white" fill-opacity="0.15"/>
    <text x="100" y="50" font-size="24" fill="white" fill-opacity="0.7" text-anchor="middle">스틸</text>
    <text x="100" y="100" font-size="48" font-weight="bold" fill="white" text-anchor="middle">%.1f</text>
  </g>
  <g transform="translate(980, 280)">
    <rect width="200" height="120" rx="12" fill="white" fill-opacity="0.15"/>
    <text x="100" y="50" font-size="24" fill="white" fill-opacity="0.7" text-anchor="middle">블록</text>
    <text x="100" y="100" font-size="48" font-weight="bold" fill="white" text-anchor="middle">%.1f</text>
  </g>
  <!-- Games played -->
  <text x="100" y="480" font-size="28" fill="white" fill-opacity="0.6">%d경기 · 효율 %.1f</text>
  <!-- Branding -->
  <text x="100" y="580" font-size="24" fill="white" fill-opacity="0.5">wkbl.win</text>
</svg>|}
    card_width card_height color1 color2
    card_width card_height
    p.name p.team_name
    p.avg_points p.avg_rebounds p.avg_assists p.avg_steals p.avg_blocks
    p.games_played p.efficiency

(** Generate game result card SVG *)
let game_card_svg ~date ~venue ~home_team ~away_team ~home_score ~away_score
    ~home_code ~away_code =
  let (home_c1, _) = team_gradient home_code in
  let (away_c1, _) = team_gradient away_code in
  let winner = if home_score > away_score then "home" else "away" in
  Printf.sprintf {|<?xml version="1.0" encoding="UTF-8"?>
<svg width="%d" height="%d" xmlns="http://www.w3.org/2000/svg">
  <defs>
    <linearGradient id="bg" x1="0%%" y1="0%%" x2="100%%" y2="100%%">
      <stop offset="0%%" style="stop-color:#0f172a"/>
      <stop offset="100%%" style="stop-color:#1e293b"/>
    </linearGradient>
  </defs>
  <rect width="%d" height="%d" fill="url(#bg)"/>
  <text x="600" y="80" font-size="32" fill="white" fill-opacity="0.7" text-anchor="middle">%s</text>
  <text x="600" y="120" font-size="24" fill="white" fill-opacity="0.5" text-anchor="middle">%s</text>
  <rect x="50" y="180" width="400" height="350" rx="20" fill="%s" fill-opacity="0.3"/>
  <text x="250" y="280" font-size="48" font-weight="bold" fill="white" text-anchor="middle">%s</text>
  <text x="250" y="420" font-size="120" font-weight="bold" fill="white" text-anchor="middle" fill-opacity="%s">%d</text>
  <text x="600" y="380" font-size="48" fill="white" fill-opacity="0.3" text-anchor="middle">VS</text>
  <rect x="750" y="180" width="400" height="350" rx="20" fill="%s" fill-opacity="0.3"/>
  <text x="950" y="280" font-size="48" font-weight="bold" fill="white" text-anchor="middle">%s</text>
  <text x="950" y="420" font-size="120" font-weight="bold" fill="white" text-anchor="middle" fill-opacity="%s">%d</text>
  <text x="600" y="580" font-size="24" fill="white" fill-opacity="0.4" text-anchor="middle">wkbl.win</text>
</svg>|}
    card_width card_height
    card_width card_height
    date venue
    away_c1 away_team (if winner = "away" then "1" else "0.6") away_score
    home_c1 home_team (if winner = "home" then "1" else "0.6") home_score

(** Convert SVG to PNG using ImageMagick (sync, blocking) *)
let svg_to_png svg_content =
  let temp_svg = Filename.temp_file "card_" ".svg" in
  let temp_png = Filename.temp_file "card_" ".png" in
  let oc = open_out temp_svg in
  output_string oc svg_content;
  close_out oc;
  let cmd = Printf.sprintf "magick -background none '%s' -density 150 '%s' 2>/dev/null"
    temp_svg temp_png in
  let result = Sys.command cmd in
  Sys.remove temp_svg;
  if result = 0 then begin
    let ic = open_in_bin temp_png in
    let len = in_channel_length ic in
    let data = Bytes.create len in
    really_input ic data 0 len;
    close_in ic;
    Sys.remove temp_png;
    Some (Bytes.to_string data)
  end else begin
    (try Sys.remove temp_png with _ -> ());
    None
  end
