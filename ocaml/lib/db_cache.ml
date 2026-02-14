(** In-memory cache with TTL and LRU eviction.

    Extracted from db.ml for modularity.
    Thread-safe for Eio concurrent access via Eio.Mutex.

    NOTE: Cache instances are created in db.ml because
    polymorphic types need usage context for type inference.
*)

(** Cache entry with expiration *)
type 'a entry = {
  value: 'a;
  expires_at: float;
}

(** Cache store with TTL, max entries, and mutex for concurrent access *)
type 'a t = {
  ttl: float;
  max_entries: int;
  store: (string, 'a entry) Hashtbl.t;
  mutex: Eio.Mutex.t;
}

(** Create a new cache instance *)
let create ~ttl ~max_entries =
  { ttl; max_entries; store = Hashtbl.create max_entries; mutex = Eio.Mutex.create () }

let now () = Unix.gettimeofday ()

(** Get value from cache if not expired.
    Acquires mutex because expired-entry removal is a write. *)
let get t key =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    match Hashtbl.find_opt t.store key with
    | Some entry when entry.expires_at > now () -> Some entry.value
    | Some _ ->
        Hashtbl.remove t.store key;
        None
    | None -> None)

(** Evict expired entries first, then oldest 25% if still over capacity.
    Caller must hold the mutex. *)
let evict_unlocked t =
  let current = now () in
  (* Phase 1: Remove expired entries *)
  let expired_keys = Hashtbl.fold (fun k entry acc ->
    if entry.expires_at <= current then k :: acc else acc
  ) t.store [] in
  List.iter (Hashtbl.remove t.store) expired_keys;
  (* Phase 2: If still over capacity, remove oldest 25% *)
  if Hashtbl.length t.store > t.max_entries then begin
    let entries = Hashtbl.fold (fun k e acc -> (k, e.expires_at) :: acc) t.store [] in
    let sorted = List.sort (fun (_, t1) (_, t2) -> Float.compare t1 t2) entries in
    let to_remove = max 1 (List.length sorted / 4) in
    List.iteri (fun i (k, _) ->
      if i < to_remove then Hashtbl.remove t.store k
    ) sorted
  end

(** Set value in cache with TTL *)
let set t key value =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Hashtbl.replace t.store key { value; expires_at = now () +. t.ttl };
    if Hashtbl.length t.store > t.max_entries then evict_unlocked t)

(** Cached function wrapper - returns Ok value from cache or computes.
    The computation [f] runs outside the mutex to avoid blocking other fibers. *)
let cached cache key f =
  (* Check cache under mutex *)
  let cached_value =
    Eio.Mutex.use_rw ~protect:true cache.mutex (fun () ->
      match Hashtbl.find_opt cache.store key with
      | Some entry when entry.expires_at > now () -> Some entry.value
      | Some _ ->
          Hashtbl.remove cache.store key;
          None
      | None -> None)
  in
  match cached_value with
  | Some value -> Ok value
  | None ->
      (* Compute outside mutex (may involve I/O) *)
      let result = f () in
      (match result with
       | Ok value -> set cache key value
       | Error _ -> ());
      result

(** Generate cache key from text (normalized, max 64 chars) *)
let cache_key_text value =
  let trimmed = String.trim value in
  let lowered = String.lowercase_ascii trimmed in
  if String.length lowered > 64 then String.sub lowered 0 64 else lowered
