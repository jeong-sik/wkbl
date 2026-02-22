(** GraphQL schema and query execution for WKBL Analytics. *)

val schema : unit Graphql.Schema.schema

val execute_query :
  ?variables:Graphql.Schema.variables ->
  ?operation_name:string ->
  string ->
  (Yojson.Basic.t, Yojson.Basic.t) result
