(** Connection pool management for Caqti/Eio. *)

open Db_common

type backend = [ `Postgres | `Sqlite | `Unknown ]

let pool_ref : (Caqti_eio.connection, Caqti_error.t) Caqti_eio.Pool.t option ref = ref None
let backend_ref : backend ref = ref `Unknown

let classify_backend db_url =
  let uri = Uri.of_string db_url in
  match Uri.scheme uri with
  | Some "postgresql" | Some "postgres" -> `Postgres
  | Some "sqlite3" | Some "sqlite" -> `Sqlite
  | _ -> `Unknown

let current_backend () = !backend_ref

(** Initialize pool with Eio context - must be called inside Eio_main.run *)
let init_pool ~sw ~stdenv db_url =
  let uri = Uri.of_string db_url in
  match Caqti_eio_unix.connect_pool ~sw ~stdenv uri with
  | Ok pool ->
      pool_ref := Some pool;
      backend_ref := classify_backend db_url;
      Ok ()
  | Error e -> Error (ConnectionFailed (Caqti_error.show e))

(** Run a database operation using the pool *)
let with_db f =
  match !pool_ref with
  | None -> Error (ConnectionFailed "Pool not initialized")
  | Some pool ->
      match Caqti_eio.Pool.use f pool with
      | Ok v -> Ok v
      | Error e -> Error (QueryFailed (Caqti_error.show e))
