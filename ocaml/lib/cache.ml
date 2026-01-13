open Lwt.Syntax

type 'a entry = {
  value: 'a;
  expires_at: float;
}

type 'a t = {
  ttl: float;
  max_entries: int;
  lock: Lwt_mutex.t;
  table: (string, 'a entry) Hashtbl.t;
}

let create ?(max_entries=256) ~ttl () =
  { ttl; max_entries; lock = Lwt_mutex.create (); table = Hashtbl.create max_entries }

let prune cache ~now =
  Hashtbl.filter_map_inplace
    (fun _ entry ->
      if entry.expires_at > now then Some entry else None)
    cache.table;
  if Hashtbl.length cache.table > cache.max_entries then (
    let overflow = Hashtbl.length cache.table - cache.max_entries in
    if overflow > 0 then (
      let removed = ref 0 in
      Hashtbl.iter (fun key _ ->
        if !removed < overflow then (
          Hashtbl.remove cache.table key;
          incr removed
        )) cache.table
    )
  )

let get cache key =
  let now = Unix.time () in
  Lwt_mutex.with_lock cache.lock (fun () ->
    match Hashtbl.find_opt cache.table key with
    | Some entry when entry.expires_at > now -> Lwt.return (Some entry.value)
    | Some _ ->
        Hashtbl.remove cache.table key;
        Lwt.return None
    | None -> Lwt.return None)

let set cache key value =
  let now = Unix.time () in
  Lwt_mutex.with_lock cache.lock (fun () ->
    prune cache ~now;
    Hashtbl.replace cache.table key { value; expires_at = now +. cache.ttl };
    Lwt.return_unit)

let get_or_compute cache ~key ~compute =
  let* cached = get cache key in
  match cached with
  | Some value -> Lwt.return value
  | None ->
      let* value = compute () in
      let* () = set cache key value in
      Lwt.return value
