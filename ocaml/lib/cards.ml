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

(** Player image URL - WKBL official site *)
let player_img_url player_id =
  Printf.sprintf "https://www.wkbl.or.kr/static/images/player/pimg/m_%s.jpg" player_id

(** EFF badge color based on value *)
let eff_color eff =
  if eff >= 20.0 then "#a855f7"  (* purple-500 *)
  else if eff >= 15.0 then "#ef4444"  (* red-500 *)
  else if eff >= 10.0 then "#22c55e"  (* green-500 *)
  else "#94a3b8"  (* slate-400 *)

(** Generate player card SVG from player_aggregate
    Gemini UX feedback applied:
    - EFF prominently displayed (WKBL MVP criteria)
    - Core stats: PTS, EFF, REB, AST
    - Profile image with circular clip *)
let player_card (p : Domain.player_aggregate) =
  let (color1, color2) = team_gradient p.team_name in
  let img_url = player_img_url p.player_id in
  let eff_col = eff_color p.efficiency in
  Printf.sprintf {|<?xml version="1.0" encoding="UTF-8"?>
<svg width="%d" height="%d" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs>
    <linearGradient id="bg" x1="0%%" y1="0%%" x2="100%%" y2="100%%">
      <stop offset="0%%" style="stop-color:%s"/>
      <stop offset="100%%" style="stop-color:%s"/>
    </linearGradient>
    <clipPath id="avatarClip">
      <circle cx="100" cy="100" r="90"/>
    </clipPath>
  </defs>
  <rect width="%d" height="%d" fill="url(#bg)"/>
  <!-- Decorative elements -->
  <circle cx="1150" cy="50" r="250" fill="white" fill-opacity="0.03"/>
  <circle cx="-30" cy="600" r="180" fill="white" fill-opacity="0.03"/>
  <!-- Profile image -->
  <g transform="translate(80, 80)">
    <circle cx="100" cy="100" r="95" fill="white" fill-opacity="0.2"/>
    <image x="10" y="10" width="180" height="180" clip-path="url(#avatarClip)"
           href="%s" preserveAspectRatio="xMidYMid slice"/>
    <circle cx="100" cy="100" r="90" fill="none" stroke="white" stroke-width="4" stroke-opacity="0.5"/>
  </g>
  <!-- Player name and team -->
  <text x="320" y="140" font-family="sans-serif" font-size="64" font-weight="bold" fill="white">%s</text>
  <text x="320" y="200" font-family="sans-serif" font-size="32" fill="white" fill-opacity="0.8">%s · %d경기</text>
  <!-- EFF highlight (primary metric) -->
  <g transform="translate(80, 320)">
    <rect width="240" height="160" rx="16" fill="%s" fill-opacity="0.25"/>
    <rect width="240" height="160" rx="16" fill="none" stroke="%s" stroke-width="3" stroke-opacity="0.6"/>
    <text x="120" y="60" font-size="24" fill="white" text-anchor="middle">효율</text>
    <text x="120" y="130" font-size="72" font-weight="bold" fill="%s" text-anchor="middle">%.1f</text>
  </g>
  <!-- Other stats: PTS, REB, AST -->
  <g transform="translate(360, 320)">
    <rect width="200" height="160" rx="12" fill="white" fill-opacity="0.12"/>
    <text x="100" y="55" font-size="22" fill="white" fill-opacity="0.7" text-anchor="middle">득점</text>
    <text x="100" y="120" font-size="56" font-weight="bold" fill="white" text-anchor="middle">%.1f</text>
  </g>
  <g transform="translate(590, 320)">
    <rect width="200" height="160" rx="12" fill="white" fill-opacity="0.12"/>
    <text x="100" y="55" font-size="22" fill="white" fill-opacity="0.7" text-anchor="middle">리바운드</text>
    <text x="100" y="120" font-size="56" font-weight="bold" fill="white" text-anchor="middle">%.1f</text>
  </g>
  <g transform="translate(820, 320)">
    <rect width="200" height="160" rx="12" fill="white" fill-opacity="0.12"/>
    <text x="100" y="55" font-size="22" fill="white" fill-opacity="0.7" text-anchor="middle">어시스트</text>
    <text x="100" y="120" font-size="56" font-weight="bold" fill="white" text-anchor="middle">%.1f</text>
  </g>
  <!-- Branding -->
  <text x="80" y="560" font-size="22" fill="white" fill-opacity="0.4">wkbl.win</text>
  <rect x="0" y="618" width="%d" height="12" fill="%s"/>
</svg>|}
    card_width card_height color1 color2
    card_width card_height
    img_url
    p.name p.team_name p.games_played
    eff_col eff_col eff_col p.efficiency
    p.avg_points p.avg_rebounds p.avg_assists
    card_width eff_col

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

(** Prediction result card for social sharing *)
let prediction_card ~(home: string) ~(away: string) (output: Domain.prediction_output) =
  let res = output.result in
  let (h_c1, _) = team_gradient home in
  let (a_c1, _) = team_gradient away in
  let winner = res.winner in
  
  Printf.sprintf {|<?xml version="1.0" encoding="UTF-8"?>
<svg width="1200" height="630" xmlns="http://www.w3.org/2000/svg">
  <defs>
    <linearGradient id="bg" x1="0%%" y1="0%%" x2="100%%" y2="100%%">
      <stop offset="0%%" style="stop-color:#0f172a"/>
      <stop offset="100%%" style="stop-color:#1e293b"/>
    </linearGradient>
  </defs>
  <rect width="1200" height="630" fill="url(#bg)"/>
  
  <!-- Left Team (Home) -->
  <rect x="0" y="0" width="600" height="630" fill="%s" opacity="0.3"/>
  <text x="300" y="250" text-anchor="middle" fill="white" font-size="60" font-weight="900" font-family="sans-serif">%s</text>
  <text x="300" y="320" text-anchor="middle" fill="white" font-size="120" font-weight="900" font-family="sans-serif" opacity="0.8">%.1f%%</text>
  
  <!-- Right Team (Away) -->
  <rect x="600" y="0" width="600" height="630" fill="%s" opacity="0.3"/>
  <text x="900" y="250" text-anchor="middle" fill="white" font-size="60" font-weight="900" font-family="sans-serif">%s</text>
  <text x="900" y="320" text-anchor="middle" fill="white" font-size="120" font-weight="900" font-family="sans-serif" opacity="0.8">%.1f%%</text>
  
  <!-- Center VS -->
  <circle cx="600" cy="315" r="80" fill="#0f172a" stroke="#f97316" stroke-width="4"/>
  <text x="600" y="335" text-anchor="middle" fill="#f97316" font-size="60" font-weight="900" font-family="sans-serif">VS</text>
  
  <!-- Winner Badge -->
  <rect x="350" y="450" width="500" height="100" rx="50" fill="#f97316"/>
  <text x="600" y="515" text-anchor="middle" fill="white" font-size="45" font-weight="900" font-family="sans-serif">%s 승리 예상</text>
  
  <!-- Footer -->
  <text x="600" y="600" text-anchor="middle" fill="#94a3b8" font-size="20" font-family="monospace">WKBL.win | AI Moneyball Analysis</text>
</svg>|}
  h_c1 home (res.prob_a *. 100.0)
  a_c1 away (res.prob_b *. 100.0)
  winner

(** Convert SVG to PNG using ImageMagick.

    This uses argv-based process spawning (no shell). Still blocking. *)
let svg_to_png svg_content =
  let temp_svg = Filename.temp_file "card_" ".svg" in
  let temp_png = Filename.temp_file "card_" ".png" in

  let write_file path content =
    let oc = open_out path in
    Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc content)
  in
  let read_file path =
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let len = in_channel_length ic in
        let data = Bytes.create len in
        really_input ic data 0 len;
        Bytes.to_string data)
  in
  let run_magick () =
    let dev_null =
      try Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 with
      | _ -> Unix.stdout
    in
    Fun.protect
      ~finally:(fun () -> if dev_null <> Unix.stdout then Unix.close dev_null)
      (fun () ->
        let argv =
          [| "magick"; "-background"; "none"; temp_svg; "-density"; "150"; temp_png |]
        in
        try
          let pid = Unix.create_process "magick" argv Unix.stdin dev_null dev_null in
          match snd (Unix.waitpid [] pid) with
          | Unix.WEXITED 0 -> true
          | _ -> false
        with
        | Unix.Unix_error (Unix.ENOENT, _, _) -> false)
  in

  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove temp_svg with _ -> ());
      (try Sys.remove temp_png with _ -> ()))
    (fun () ->
      try
        write_file temp_svg svg_content;
        if run_magick () then Some (read_file temp_png) else None
      with
      | _ -> None)
