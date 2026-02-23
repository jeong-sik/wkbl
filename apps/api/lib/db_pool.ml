(** Connection pool management for Caqti/Eio. *)

open Db_common

let pool_ref : (Caqti_eio.connection, Caqti_error.t) Caqti_eio.Pool.t option ref = ref None

(** Initialize pool with Eio context - must be called inside Eio_main.run *)
let init_pool ~sw ~stdenv db_url =
  let uri = Uri.of_string db_url in
  match Caqti_eio_unix.connect_pool ~sw ~stdenv uri with
  | Ok pool -> pool_ref := Some pool; Ok ()
  | Error e -> Error (ConnectionFailed (Caqti_error.show e))

(** Run a database operation using the pool *)
let with_db f =
  match !pool_ref with
  | None -> Error (ConnectionFailed "Pool not initialized")
  | Some pool ->
      match Caqti_eio.Pool.use f pool with
      | Ok v -> Ok v
      | Error e -> Error (QueryFailed (Caqti_error.show e))
