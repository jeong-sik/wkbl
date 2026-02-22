(** Background backfill modules for PBP events and player metadata.
    These spawn Eio fibers to scrape missing data without blocking HTTP responses. *)

(* Runtime config from environment variables *)
let poller_interval =
  Sys.getenv_opt "POLLER_INTERVAL_SECS"
  |> Option.map float_of_string
  |> Option.value ~default:60.0

let fetch_timeout =
  Sys.getenv_opt "FETCH_TIMEOUT_SECS"
  |> Option.map float_of_string
  |> Option.value ~default:10.0

module Pbp = struct
  (* Guardrails:
     - Never block HTTP responses on network scraping.
     - Deduplicate concurrent backfills per game_id.
     - Cooldown avoids repeated upstream hits when data is persistently missing. *)
  let inflight : (string, unit) Hashtbl.t = Hashtbl.create 256
  let last_attempt : (string, float) Hashtbl.t = Hashtbl.create 256

  let cooldown_seconds = poller_interval
  let timeout_seconds = fetch_timeout

  let parse_game_id (id : string) : (string * string * int) option =
    match String.split_on_char '-' (String.trim id) with
    | [season_gu; game_type; game_no_s] -> (
        match int_of_string_opt (String.trim game_no_s) with
        | Some game_no -> Some (String.trim season_gu, String.trim game_type, game_no)
        | None -> None)
    | _ -> None

  let maybe_spawn ~sw ~env ~(game_id : string) ~(tag : string) () : unit =
    match parse_game_id game_id with
    | None -> ()
    | Some (season_gu, game_type, game_no) ->
        let now = Unix.time () in
        if Hashtbl.mem inflight game_id then ()
        else (
          match Hashtbl.find_opt last_attempt game_id with
          | Some last when now -. last < cooldown_seconds -> ()
          | _ ->
              Hashtbl.replace inflight game_id ();
              Hashtbl.replace last_attempt game_id now;
              let clock = Eio.Stdenv.clock env in
              Eio.Fiber.fork ~sw (fun () ->
                Fun.protect
                  ~finally:(fun () -> Hashtbl.remove inflight game_id)
                  (fun () ->
                    try
                      let events_opt =
                        try
                          Some
                            (Eio.Time.with_timeout_exn clock timeout_seconds (fun () ->
                               Scraper.fetch_game_pbp_events ~sw ~env ~season_gu ~game_type
                                 ~game_no ()))
                        with
                        | Eio.Time.Timeout -> None
                      in
                      (match events_opt with
                      | None ->
                          Printf.eprintf "[%s] backfill fetch timeout (%s)\n%!" tag game_id
                      | Some events ->
                          if events <> [] then (
                            let rows =
                              events |> List.map (fun (e : Domain.pbp_event) -> (e, None))
                            in
                            match Db_sync.replace_pbp_events ~game_id rows with
                            | Ok _n -> ()
                            | Error e ->
                                Printf.eprintf "[%s] backfill DB error (%s): %s\n%!"
                                  tag
                                  game_id
                                  (Db.show_db_error e)
                          ))
                    with
                    | exn ->
                        Printf.eprintf "[%s] backfill error (%s): %s\n%!"
                          tag
                          game_id
                          (Printexc.to_string exn)
                  )
              )
        )
end

module Player_meta = struct
  (* Guardrails:
     - Never block HTTP responses on network scraping.
     - Deduplicate concurrent fetches per player_id.
     - Cooldown avoids repeated upstream hits when data is persistently missing. *)
  let inflight : (string, unit) Hashtbl.t = Hashtbl.create 256
  let last_attempt : (string, float) Hashtbl.t = Hashtbl.create 256

  let cooldown_seconds = 6.0 *. 60.0 *. 60.0
  let timeout_seconds = 10.0

  let maybe_spawn ~sw ~env ~(player_id : string) ~(tag : string) () : unit =
    let pid = String.trim player_id in
    if pid = "" then ()
    else
      let now = Unix.time () in
      if Hashtbl.mem inflight pid then ()
      else (
        match Hashtbl.find_opt last_attempt pid with
        | Some last when now -. last < cooldown_seconds -> ()
        | _ ->
            Hashtbl.replace inflight pid ();
            Hashtbl.replace last_attempt pid now;
            let clock = Eio.Stdenv.clock env in
            Eio.Fiber.fork ~sw (fun () ->
              Fun.protect
                ~finally:(fun () -> Hashtbl.remove inflight pid)
                (fun () ->
                  try
                    let info_opt =
                      try
                        Eio.Time.with_timeout_exn clock timeout_seconds (fun () ->
                          Scraper.fetch_player_detail ~sw ~env ~player_id:pid)
                      with
                      | Eio.Time.Timeout -> None
                    in
                    match info_opt with
                    | None -> ()
                    | Some info -> (
                        match Db_sync.upsert_player_info info with
                        | Ok () -> ()
                        | Error e ->
                            Printf.eprintf "[%s] player meta DB error (%s): %s\n%!"
                              tag
                              pid
                              (Db.show_db_error e))
                  with
                  | exn ->
                      Printf.eprintf "[%s] player meta error (%s): %s\n%!"
                        tag
                        pid
                        (Printexc.to_string exn)
                )
            )
      )
end

(** Build a lookup map from player_id -> player_info *)
let build_player_info_map (infos: Domain.player_info list) =
  let map = Hashtbl.create (List.length infos * 2 + 1) in
  infos |> List.iter (fun (p: Domain.player_info) ->
    Hashtbl.replace map p.id p
  );
  map

let get_player_info_map () =
  match Db.get_all_player_info () with
  | Ok infos -> Some (build_player_info_map infos)
  | Error _ -> None

let player_meta_missing (info_opt : Domain.player_info option) : bool =
  match info_opt with
  | None -> true
  | Some info -> info.position = None && info.birth_date = None && info.height = None

let backfill_duplicate_player_meta ~sw ~env ~(players : Domain.player_aggregate list)
    (player_info_map : (string, Domain.player_info) Hashtbl.t option) : unit =
  match player_info_map with
  | None -> ()
  | Some map ->
      let name_counts : (string, int) Hashtbl.t = Hashtbl.create 64 in
      players |> List.iter (fun (p : Domain.player_aggregate) ->
        let key = Views_common.normalize_name p.name in
        let prev = Hashtbl.find_opt name_counts key |> Option.value ~default:0 in
        Hashtbl.replace name_counts key (prev + 1)
      );
      players |> List.iter (fun (p : Domain.player_aggregate) ->
        let key = Views_common.normalize_name p.name in
        let is_dup =
          match Hashtbl.find_opt name_counts key with
          | Some c when c > 1 -> true
          | _ -> false
        in
        if is_dup then (
          let info_opt = Hashtbl.find_opt map p.player_id in
          if player_meta_missing info_opt then
            Player_meta.maybe_spawn ~sw ~env ~player_id:p.player_id ~tag:"PLAYERS" ()
        )
      )
