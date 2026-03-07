(** AI-powered analysis and explanations for WKBL predictions *)

open Domain
open I18n

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
        let what_if_text = 
          match ctx.pcb_what_if_missing with
          | [] -> ""
          | wim :: _ -> 
              Printf.sprintf "\n[WHAT-IF 시나리오 가동 중]: 만약 %s의 핵심 에이스 %s 선수가 결장한다면을 가정한 시뮬레이션입니다! 해설 시 이 가상의 상황을 반드시 극적으로 언급해주세요." wim.wim_team wim.wim_player_name
        in
        Printf.sprintf "\n컨텍스트: %s. %s%s" form_text rest_text what_if_text
  in

  Printf.sprintf
 {|당신은 한국 여자프로농구(WKBL) 전문 해설가입니다. 다음 경기 예측 결과를 바탕으로 2-3문장의 자연스러운 해설을 작성하세요.

## 경기 정보
- 홈: %s
- 원정: %s
- 중립 경기: %s

## 예측 결과
- 승리 예상: %s (%.1f%%)
- 패배 예상: %s (%.1f%%)

## 분석 근거
- 전력(최근 경기 결과 기반): %s %.0f vs %s %.0f
- 득실 기대(득점/실점 기반): %s %.1f%% vs %s %.1f%%
- 기록 기반 확률: %.1f%%%s

## 요청
- 승부 예측과 핵심 근거를 간결하게 설명
- 전문 용어(ELO, 피타고리안 등) 대신 쉬운 한국어로 설명
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

(** Call LLM via MCP JSON-RPC to llm-mcp server *)
let llm_mcp_url =
  Sys.getenv_opt "WKBL_LLM_MCP_URL"
  |> Option.value ~default:"http://localhost:8932/mcp"

(* [call_llm_http] is invoked from within the main Eio event-loop (Kirin server).
   Avoid shelling-out and avoid nested [Eio_main.run] by taking the shared Eio
   context from the server on startup. *)
let llm_post_json : (string -> (string, string) result) option ref = ref None

let set_llm_ctx ~sw ~env =
  llm_post_json := Some (fun body_str ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    let uri = Uri.of_string llm_mcp_url in
    let headers = Cohttp.Header.of_list [
      ("Content-Type", "application/json");
      ("Accept", "application/json");
    ] in
    let client = Cohttp_eio.Client.make ~https:None net in
    try
      let resp, resp_body_str =
        Eio.Time.with_timeout_exn clock 30.0 (fun () ->
          let resp, body =
            Cohttp_eio.Client.post
              ~sw
              client
              ~headers
              ~body:(Cohttp_eio.Body.of_string body_str)
              uri
          in
          let resp_body_str =
            Eio.Buf_read.(parse_exn take_all) body
              ~max_size:(5 * 1024 * 1024)
          in
          (resp, resp_body_str)
        )
      in
      let code =
        resp
        |> Cohttp.Response.status
        |> Cohttp.Code.code_of_status
      in
      if code >= 200 && code < 300 then
        Ok resp_body_str
      else
        Error (Printf.sprintf "LLM HTTP %d: %s" code (String.trim resp_body_str))
    with
    | Eio.Time.Timeout -> Error "LLM HTTP timeout"
    | exn -> Error (Printexc.to_string exn))

let clear_llm_ctx () =
  llm_post_json := None

let call_llm_http ~prompt =
  try
    (* Build MCP JSON-RPC request for gemini tool *)
    let mcp_request = Yojson.Safe.to_string (`Assoc [
      ("jsonrpc", `String "2.0");
      ("method", `String "tools/call");
      ("id", `Int 1);
      ("params", `Assoc [
        ("name", `String "gemini");
        ("arguments", `Assoc [
          ("prompt", `String prompt);
          ("max_tokens", `Int 200);
        ]);
      ]);
    ]) in
    let response =
      match !llm_post_json with
      | None ->
          Error "LLM client not configured (Ai.set_llm_ctx not called)"
      | Some post_json -> post_json mcp_request
    in

    match response with
    | Error _ as e -> e
    | Ok response ->

    (* Parse MCP JSON-RPC response *)
    match Yojson.Safe.from_string response with
    | `Assoc fields ->
        (match List.assoc_opt "result" fields with
        | Some (`Assoc result_fields) ->
            (match List.assoc_opt "isError" result_fields with
            | Some (`Bool true) -> Error "LLM returned error"
            | _ ->
                match List.assoc_opt "content" result_fields with
                | Some (`List contents) ->
                    (* Extract text from first content item *)
                    let texts = List.filter_map (function
                      | `Assoc c ->
                          (match List.assoc_opt "text" c with
                          | Some (`String t) ->
                              (* Remove [Extra] metadata if present *)
                              let t = match String.index_opt t '\n' with
                                | Some i when String.length t > i + 1 &&
                                    String.sub t (i+1) (min 7 (String.length t - i - 1)) = "[Extra]" ->
                                    String.sub t 0 i
                                | _ -> t
                              in Some (String.trim t)
                          | _ -> None)
                      | _ -> None
                    ) contents in
                    (match texts with
                    | t :: _ -> Ok t
                    | [] -> Error "No text in response")
                | _ -> Error "No content array in result")
        | _ ->
            match List.assoc_opt "error" fields with
            | Some (`Assoc err) ->
                let msg = match List.assoc_opt "message" err with
                  | Some (`String m) -> m | _ -> "Unknown error" in
                Error msg
            | _ -> Error "Invalid MCP response")
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
    if elo_diff > 100.0 then Printf.sprintf "전력 지표에서 %s%s 크게 앞서며" loser (particle_reul loser)
    else if elo_diff > 50.0 then Printf.sprintf "전력 지표에서 %s%s 앞서며" loser (particle_reul loser)
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
  | first :: _ ->
      let best = List.fold_left (fun best p ->
        (* Score = PTS + (REB + AST) * 1.5 for MVP calculation *)
        let score p = float_of_int p.bs_pts +. (float_of_int (p.bs_reb + p.bs_ast)) *. 1.5 in
        if score p > score best then p else best
      ) first players in
      Some best

(** Format quarter scores for prompt *)
let format_quarter_flow (quarters: quarter_score list) home_name away_name =
  if quarters = [] then ""
  else
    let lines = List.map (fun q ->
      Printf.sprintf "- %s: %s %d - %s %d"
        q.qs_period home_name q.qs_home_score away_name q.qs_away_score
    ) quarters in
    String.concat "\n" lines

(** Analyze game flow from quarter scores *)
let analyze_flow (quarters: quarter_score list) =
  match quarters with
  | [] -> ""
  | _ ->
      (* Find momentum shifts *)
      let rec find_shifts prev_diff = function
        | [] -> []
        | q :: rest ->
            let diff = q.qs_home_score - q.qs_away_score in
            let shift = if prev_diff * diff < 0 then [q.qs_period] else [] in
            shift @ find_shifts diff rest
      in
      let shifts = match quarters with
        | q :: rest -> find_shifts (q.qs_home_score - q.qs_away_score) rest
        | [] -> []
      in
      (* Calculate quarter-by-quarter scoring (O(n) via shared helper) *)
      let q_scores = Views_common.quarter_deltas quarters in
      (* Find best quarter *)
      let best_q = List.fold_left (fun acc (period, home_q, away_q) ->
        let diff = abs (home_q - away_q) in
        match acc with
        | None -> Some (period, diff, home_q > away_q)
        | Some (_, best_diff, _) -> if diff > best_diff then Some (period, diff, home_q > away_q) else acc
      ) None q_scores in
      let flow_text = match shifts with
        | [] -> "한 팀이 시종일관 리드"
        | [_] -> "역전이 있었음"
        | _ -> Printf.sprintf "%d번의 리드 체인지" (List.length shifts)
      in
      let best_text = match best_q with
        | Some (period, diff, is_home) ->
            Printf.sprintf ", %s에서 %s%s %d점 차 아웃스코어"
              period (if is_home then "홈팀" else "어웨이팀") (if is_home then "이" else "이") diff
        | None -> ""
      in
      flow_text ^ best_text

(** Generate game summary prompt for LLM *)
let build_game_summary_prompt ?(quarters=[]) ?(lang=Ko) (bs: game_boxscore) =
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
    |> (function [] -> [] | x :: _ -> [x])
  in

  let mvp_text = match mvp_candidates with
    | [p] -> Printf.sprintf "MVP 후보: %s (%dPTS, %dREB, %dAST)"
        p.bs_player_name p.bs_pts p.bs_reb p.bs_ast
    | _ -> ""
  in

  (* Quarter flow section *)
  let quarter_section = if quarters = [] then "" else
    match lang with
    | Ko ->
        Printf.sprintf "\n\n## 쿼터별 점수\n%s\n\n## 경기 흐름\n%s"
          (format_quarter_flow quarters gi.gi_home_team_name gi.gi_away_team_name)
          (analyze_flow quarters)
    | En ->
        Printf.sprintf "\n\n## Quarter Scores\n%s\n\n## Game Flow\n%s"
          (format_quarter_flow quarters gi.gi_home_team_name gi.gi_away_team_name)
          (analyze_flow quarters)
  in

  match lang with
  | Ko ->
      Printf.sprintf
        {|당신은 한국 여자프로농구(WKBL) 전문 기자입니다. 다음 경기 결과를 바탕으로 3문장 이내의 간결한 경기 요약을 작성하세요.

## 경기 정보
- 날짜: %s
- 홈: %s (%d점)
- 어웨이: %s (%d점)
- 승자: %s (점수차: %d점)

## 주요 선수
%s%s

## 요청
- 첫 문장: 승패 결과와 점수차
- 둘째 문장: MVP 후보의 활약
- 셋째 문장: 경기 흐름/특징 (예: 접전, 대승, 역전, 3쿼터 폭발 등)
- 팀명은 정식 명칭 사용
- 과도한 확신 표현 자제
- 3문장 이내로 작성|}
        gi.gi_game_date
        gi.gi_home_team_name gi.gi_home_score
        gi.gi_away_team_name gi.gi_away_score
        winner (abs margin)
        mvp_text quarter_section
  | En ->
      Printf.sprintf
        {|You are a WKBL basketball reporter. Based on the game result below, write a concise recap in at most 3 sentences.

## Game Info
- Date: %s
- Home: %s (%d)
- Away: %s (%d)
- Winner: %s (margin: %d)

## Key Player
%s%s

## Request
- Sentence 1: result and margin
- Sentence 2: top performer impact
- Sentence 3: flow / turning point (optional)
- Use official team names
- Avoid overconfident language
- Max 3 sentences|}
        gi.gi_game_date
        gi.gi_home_team_name gi.gi_home_score
        gi.gi_away_team_name gi.gi_away_score
        winner (abs margin)
        mvp_text quarter_section

(** Fallback game summary when AI is unavailable *)
let fallback_game_summary ?(lang=Ko) (bs: game_boxscore) =
  let gi = bs.boxscore_game in
  let score_home = gi.gi_home_score in
  let score_away = gi.gi_away_score in
  let has_player_stats =
    bs.boxscore_home_players <> [] || bs.boxscore_away_players <> []
  in
  let has_result = score_home > 0 && score_away > 0 && score_home <> score_away in
  let margin = abs (score_home - score_away) in

  (* If we do not have a meaningful result yet, be explicit instead of guessing. *)
  if (not has_result) && (not has_player_stats) then
    (match lang with
    | Ko -> "경기 전이거나 기록이 아직 없습니다. 경기 후에 업데이트됩니다."
    | En -> "This game hasn't started yet or stats aren't available. It will update after the game.")
  else if not has_result then
    (match lang with
    | Ko -> "기록이 아직 완전하지 않아 요약을 보류합니다. 잠시 후 다시 확인해 주세요."
    | En -> "The result isn't fully available yet, so the summary is withheld for now.")
  else
    let winner, loser =
      if score_home > score_away then
        (gi.gi_home_team_name, gi.gi_away_team_name)
      else
        (gi.gi_away_team_name, gi.gi_home_team_name)
    in

  (* Find MVP candidate *)
  let all_players = bs.boxscore_home_players @ bs.boxscore_away_players in
  let mvp = find_top_performer all_players in

  let result_text =
    match lang with
    | Ko ->
        if margin >= 20 then
          Printf.sprintf "%s%s %s%s 상대로 %d점 차 대승을 거뒀습니다."
            winner (particle_ga winner) loser (particle_reul loser) margin
        else if margin >= 10 then
          Printf.sprintf "%s%s %s%s %d점 차로 제압했습니다."
            winner (particle_ga winner) loser (particle_reul loser) margin
        else if margin <= 5 then
          Printf.sprintf "%s%s %s%s 상대로 %d점 차 접전 끝에 승리했습니다."
            winner (particle_ga winner) loser (particle_reul loser) margin
        else
          Printf.sprintf "%s%s %s%s %d점 차로 이겼습니다."
            winner (particle_ga winner) loser (particle_reul loser) margin
    | En ->
        if margin >= 20 then
          Printf.sprintf "%s beat %s by %d points." winner loser margin
        else if margin >= 10 then
          Printf.sprintf "%s defeated %s by %d points." winner loser margin
        else if margin <= 5 then
          Printf.sprintf "%s edged %s by %d points." winner loser margin
        else
          Printf.sprintf "%s won by %d points over %s." winner margin loser
  in

  let mvp_text = match mvp with
    | Some p ->
        (match lang with
        | Ko ->
            let double_double =
              (if p.bs_pts >= 10 then 1 else 0)
              + (if p.bs_reb >= 10 then 1 else 0)
              + (if p.bs_ast >= 10 then 1 else 0)
            in
            if double_double >= 2 then
              Printf.sprintf "%s%s %d점-%d리바운드-%d어시스트 더블더블로 승리에 힘을 보탰습니다."
                p.bs_player_name (particle_ga p.bs_player_name) p.bs_pts p.bs_reb p.bs_ast
            else if p.bs_pts >= 20 then
              Printf.sprintf "%s%s %d점으로 득점을 이끌었습니다."
                p.bs_player_name (particle_ga p.bs_player_name) p.bs_pts
            else
              Printf.sprintf "%s%s %d점, %d리바운드, %d어시스트로 활약했습니다."
                p.bs_player_name (particle_ga p.bs_player_name) p.bs_pts p.bs_reb p.bs_ast
        | En ->
            Printf.sprintf "Top performer: %s with %d points, %d rebounds, and %d assists."
              p.bs_player_name p.bs_pts p.bs_reb p.bs_ast)
    | None -> ""
  in

  if mvp_text <> "" then result_text ^ " " ^ mvp_text
  else result_text

(** Generate AI game summary *)
let generate_game_summary ?(lang=Ko) (bs: game_boxscore) =
  let key = Printf.sprintf "%s:%s" bs.boxscore_game.gi_game_id (lang_to_code lang) in

  (* Check cache first *)
  match Hashtbl.find_opt game_summary_cache key with
  | Some cached -> cached
  | None ->
      let summary =
        (* Fetch quarter scores for game flow analysis *)
        let quarters = match Db.get_quarter_scores bs.boxscore_game.gi_game_id with
          | Ok qs -> qs
          | Error _ -> []
        in
        (* If the score is not meaningful yet (ex: 0-0 scheduled games), do not call the LLM. *)
        let gi = bs.boxscore_game in
        let has_player_stats =
          bs.boxscore_home_players <> [] || bs.boxscore_away_players <> []
        in
        let has_result =
          gi.gi_home_score > 0
          && gi.gi_away_score > 0
          && gi.gi_home_score <> gi.gi_away_score
        in
        if (not has_result) && (not has_player_stats) then
          fallback_game_summary ~lang bs
        else
          let prompt = build_game_summary_prompt ~quarters ~lang bs in
          match call_llm_http ~prompt with
          | Ok explanation ->
              Hashtbl.replace game_summary_cache key explanation;
              explanation
          | Error _ -> fallback_game_summary ~lang bs
      in
      Hashtbl.replace game_summary_cache key summary;
      summary
