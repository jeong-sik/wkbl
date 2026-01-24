(** AI-powered analysis and explanations for WKBL predictions *)

open Domain

(** Simple in-memory cache for AI explanations *)
let explanation_cache : (string, string) Hashtbl.t = Hashtbl.create 32

(** Generate cache key from prediction parameters *)
let cache_key ~home ~away ~prob =
  Printf.sprintf "%s_vs_%s_%.0f" home away (prob *. 100.0)

(** Build prompt for prediction explanation *)
let build_prediction_prompt ~(home: string) ~(away: string) (output: prediction_output) =
  let r = output.result in
  let b = output.breakdown in
  let winner_prob = if r.prob_a > r.prob_b then r.prob_a else r.prob_b in
  let loser = if r.winner = home then away else home in

  (* Context factors if available *)
  let context_text = match b.pb_context with
    | None -> ""
    | Some ctx ->
        let form_text =
          if ctx.pcb_form_delta > 0.02 then
            Printf.sprintf "최근 폼: %s이 더 좋음 (+%.1f%%)" home (ctx.pcb_form_delta *. 100.0)
          else if ctx.pcb_form_delta < -0.02 then
            Printf.sprintf "최근 폼: %s이 더 좋음 (+%.1f%%)" away (Float.abs ctx.pcb_form_delta *. 100.0)
          else "최근 폼: 비슷함"
        in
        let rest_text = match ctx.pcb_rest_home_days, ctx.pcb_rest_away_days with
          | Some h, Some a when h > a + 1 -> Printf.sprintf "휴식: %s가 %d일 더 쉼" home (h - a)
          | Some h, Some a when a > h + 1 -> Printf.sprintf "휴식: %s가 %d일 더 쉼" away (a - h)
          | _ -> ""
        in
        Printf.sprintf "\n컨텍스트: %s. %s" form_text rest_text
  in

  Printf.sprintf
{|당신은 한국 여자프로농구(WKBL) 전문 해설가입니다. 다음 경기 예측 결과를 바탕으로 2-3문장의 자연스러운 해설을 작성하세요.

## 경기 정보
- 홈: %s
- 어웨이: %s
- 중립 경기: %s

## 예측 결과
- 승리 예상: %s (%.1f%%)
- 패배 예상: %s (%.1f%%)

## 분석 근거
- ELO 레이팅: %s %.0f vs %s %.0f
- 피타고리안 승률: %s %.1f%% vs %s %.1f%%
- 스탯 기반 확률: %.1f%%%s

## 요청
- 승부 예측과 핵심 근거를 간결하게 설명
- 팀 이름은 정식 명칭 사용 (예: KB스타즈, 삼성생명)
- 과도한 확신 표현 자제
- 2-3문장으로 작성|}
    home away
    (if b.pb_is_neutral then "예" else "아니오")
    r.winner (winner_prob *. 100.0)
    loser ((1.0 -. winner_prob) *. 100.0)
    home b.pb_elo_home away b.pb_elo_away
    home (b.pb_pyth_home *. 100.0) away (b.pb_pyth_away *. 100.0)
    (b.pb_stats_prob *. 100.0)
    context_text

(** Call LLM via HTTP to llm-mcp server *)
let call_llm_http ~prompt =
  try
    (* Use curl to call llm-mcp Gemini endpoint *)
    let cmd = Printf.sprintf
      "curl -s -X POST http://localhost:8932/gemini -H 'Content-Type: application/json' -d '%s' 2>/dev/null"
      (Yojson.Safe.to_string (`Assoc [
        ("prompt", `String prompt);
        ("response_format", `String "compact");
        ("max_tokens", `Int 200);
      ]))
    in
    let ic = Unix.open_process_in cmd in
    let response = In_channel.input_all ic in
    let _ = Unix.close_process_in ic in

    (* Parse JSON response *)
    match Yojson.Safe.from_string response with
    | `Assoc fields ->
        (match List.assoc_opt "content" fields with
        | Some (`String content) -> Ok content
        | _ ->
            match List.assoc_opt "response" fields with
            | Some (`String content) -> Ok content
            | _ -> Error "No content in response")
    | _ -> Error "Invalid JSON response"
  with
  | e -> Error (Printexc.to_string e)

(** Generate AI explanation for a prediction *)
let generate_explanation ~(home: string) ~(away: string) (output: prediction_output) =
  let key = cache_key ~home ~away ~prob:output.result.prob_a in

  (* Check cache first *)
  match Hashtbl.find_opt explanation_cache key with
  | Some cached -> Ok cached
  | None ->
      let prompt = build_prediction_prompt ~home ~away output in
      match call_llm_http ~prompt with
      | Ok explanation ->
          (* Cache the result *)
          Hashtbl.replace explanation_cache key explanation;
          Ok explanation
      | Error e -> Error e

(** Check if Korean character has final consonant (받침) *)
let has_batchim s =
  if String.length s = 0 then false
  else
    (* Get last UTF-8 character - Korean chars are 3 bytes *)
    let len = String.length s in
    if len >= 3 then
      let last_char = String.sub s (len - 3) 3 in
      let b0 = Char.code last_char.[0] in
      let b1 = Char.code last_char.[1] in
      let b2 = Char.code last_char.[2] in
      (* Decode UTF-8 to Unicode codepoint *)
      let codepoint = ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F) in
      let hangul_base = 0xAC00 in
      let hangul_end = 0xD7A3 in
      (* Check if it's a Hangul syllable and has jongseong (final consonant) *)
      if codepoint >= hangul_base && codepoint <= hangul_end then
        let normalized = codepoint - hangul_base in
        (normalized mod 28) <> 0  (* jongseong index 0 means no final consonant *)
      else
        false  (* Not a Hangul syllable *)
    else false

(** Get subject particle (이/가) based on final consonant *)
let particle_ga name = if has_batchim name then "이" else "가"

(** Get object particle (을/를) based on final consonant *)
let particle_reul name = if has_batchim name then "을" else "를"

(** Fallback explanation when AI is unavailable *)
let fallback_explanation ~(home: string) ~(away: string) (output: prediction_output) =
  let r = output.result in
  let b = output.breakdown in
  let winner_prob = if r.prob_a > r.prob_b then r.prob_a else r.prob_b in
  let loser = if r.winner = home then away else home in
  let confidence =
    if winner_prob > 0.7 then "유력한"
    else if winner_prob > 0.6 then "우세한"
    else "박빙의"
  in

  let elo_diff = Float.abs (b.pb_elo_home -. b.pb_elo_away) in
  let elo_factor =
    if elo_diff > 100.0 then Printf.sprintf "ELO 레이팅에서 %s%s 크게 앞서며" loser (particle_reul loser)
    else if elo_diff > 50.0 then Printf.sprintf "ELO 레이팅에서 %s%s 앞서며" loser (particle_reul loser)
    else ""
  in

  let venue = if b.pb_is_neutral then "중립 경기에서" else Printf.sprintf "%s 홈에서" home in

  Printf.sprintf "%s%s %s %s 승리가 예상됩니다 (%.0f%%). %s시즌 성적과 최근 폼을 종합한 분석입니다."
    r.winner (particle_ga r.winner) venue confidence (winner_prob *. 100.0)
    (if elo_factor <> "" then elo_factor ^ " " else "")

(** Get explanation with fallback *)
let get_explanation ~(home: string) ~(away: string) (output: prediction_output) =
  match generate_explanation ~home ~away output with
  | Ok explanation -> explanation
  | Error _ -> fallback_explanation ~home ~away output

(* ========== Game Summary (Phase 4.2) ========== *)

(** Cache for game summaries *)
let game_summary_cache : (string, string) Hashtbl.t = Hashtbl.create 32

(** Find top performer from player list *)
let find_top_performer (players: boxscore_player_stat list) =
  match players with
  | [] -> None
  | _ ->
      let best = List.fold_left (fun best p ->
        (* Score = PTS + (REB + AST) * 1.5 for MVP calculation *)
        let score p = float_of_int p.bs_pts +. (float_of_int (p.bs_reb + p.bs_ast)) *. 1.5 in
        if score p > score best then p else best
      ) (List.hd players) players in
      Some best

(** Generate game summary prompt for LLM *)
let build_game_summary_prompt (bs: game_boxscore) =
  let gi = bs.boxscore_game in
  let margin = gi.gi_home_score - gi.gi_away_score in
  let winner =
    if margin > 0 then gi.gi_home_team_name
    else gi.gi_away_team_name
  in

  let home_top = find_top_performer bs.boxscore_home_players in
  let away_top = find_top_performer bs.boxscore_away_players in

  let top_performers = [home_top; away_top] |> List.filter_map Fun.id in
  let mvp_candidates = top_performers
    |> List.sort (fun a b ->
        let score p = float_of_int p.bs_pts +. (float_of_int (p.bs_reb + p.bs_ast)) *. 1.5 in
        compare (score b) (score a))
    |> (fun l -> try [List.hd l] with _ -> [])
  in

  let mvp_text = match mvp_candidates with
    | [p] -> Printf.sprintf "MVP 후보: %s (%dPTS, %dREB, %dAST)"
        p.bs_player_name p.bs_pts p.bs_reb p.bs_ast
    | _ -> ""
  in

  Printf.sprintf
{|당신은 한국 여자프로농구(WKBL) 전문 기자입니다. 다음 경기 결과를 바탕으로 3문장 이내의 간결한 경기 요약을 작성하세요.

## 경기 정보
- 날짜: %s
- 홈: %s (%d점)
- 어웨이: %s (%d점)
- 승자: %s (점수차: %d점)

## 주요 선수
%s

## 요청
- 첫 문장: 승패 결과와 점수차
- 둘째 문장: MVP 후보의 활약
- 셋째 문장: 경기 특징 (예: 접전, 대승, 역전 등)
- 팀명은 정식 명칭 사용
- 3문장 이내로 작성|}
    gi.gi_game_date
    gi.gi_home_team_name gi.gi_home_score
    gi.gi_away_team_name gi.gi_away_score
    winner (abs margin)
    mvp_text

(** Fallback game summary when AI is unavailable *)
let fallback_game_summary (bs: game_boxscore) =
  let gi = bs.boxscore_game in
  let margin = abs (gi.gi_home_score - gi.gi_away_score) in
  let winner, loser =
    if gi.gi_home_score > gi.gi_away_score then
      (gi.gi_home_team_name, gi.gi_away_team_name)
    else
      (gi.gi_away_team_name, gi.gi_home_team_name)
  in

  (* Find MVP candidate *)
  let all_players = bs.boxscore_home_players @ bs.boxscore_away_players in
  let mvp = find_top_performer all_players in

  let result_text =
    if margin >= 20 then
      Printf.sprintf "%s%s %s%s 상대로 %d점 차 대승을 거뒀습니다."
        winner (particle_ga winner) loser (particle_reul loser) margin
    else if margin >= 10 then
      Printf.sprintf "%s%s %s%s %d점 차로 제압했습니다."
        winner (particle_ga winner) loser (particle_reul loser) margin
    else if margin <= 5 then
      Printf.sprintf "%s%s %s%s 상대로 %d점 차 접전 끝에 신승을 거뒀습니다."
        winner (particle_ga winner) loser (particle_reul loser) margin
    else
      Printf.sprintf "%s%s %s%s %d점 차로 이겼습니다."
        winner (particle_ga winner) loser (particle_reul loser) margin
  in

  let mvp_text = match mvp with
    | Some p ->
        let double_double =
          (if p.bs_pts >= 10 then 1 else 0) +
          (if p.bs_reb >= 10 then 1 else 0) +
          (if p.bs_ast >= 10 then 1 else 0)
        in
        if double_double >= 2 then
          Printf.sprintf "%s%s %dPTS-%dREB-%dAST 더블더블로 팀 승리를 이끌었습니다."
            p.bs_player_name (particle_ga p.bs_player_name) p.bs_pts p.bs_reb p.bs_ast
        else if p.bs_pts >= 20 then
          Printf.sprintf "%s%s %d득점 맹활약으로 팀 공격을 주도했습니다."
            p.bs_player_name (particle_ga p.bs_player_name) p.bs_pts
        else
          Printf.sprintf "%s%s %dPTS, %dREB, %dAST를 기록하며 활약했습니다."
            p.bs_player_name (particle_ga p.bs_player_name) p.bs_pts p.bs_reb p.bs_ast
    | None -> ""
  in

  if mvp_text <> "" then result_text ^ " " ^ mvp_text
  else result_text

(** Generate AI game summary *)
let generate_game_summary (bs: game_boxscore) =
  let key = bs.boxscore_game.gi_game_id in

  (* Check cache first *)
  match Hashtbl.find_opt game_summary_cache key with
  | Some cached -> cached
  | None ->
      let summary =
        let prompt = build_game_summary_prompt bs in
        match call_llm_http ~prompt with
        | Ok explanation ->
            Hashtbl.replace game_summary_cache key explanation;
            explanation
        | Error _ -> fallback_game_summary bs
      in
      Hashtbl.replace game_summary_cache key summary;
      summary
