(** Oneshot query wrappers for PgBouncer compatibility.

    PgBouncer in transaction mode doesn't support prepared statements.
    These wrappers default oneshot=true on all Caqti request operators.
*)

include Caqti_request.Infix

let ( ->. ) pt rt ?(oneshot = true) s =
  Caqti_request.Infix.( ->. ) pt rt ~oneshot s

let ( ->? ) pt rt ?(oneshot = true) s =
  Caqti_request.Infix.( ->? ) pt rt ~oneshot s

let ( ->* ) pt rt ?(oneshot = true) s =
  Caqti_request.Infix.( ->* ) pt rt ~oneshot s
