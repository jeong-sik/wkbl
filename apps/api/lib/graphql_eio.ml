open Eio.Std

module Future = struct
  type 'a t = 'a Eio.Promise.t

  let create () =
    let p, r = Eio.Promise.create () in
    (p, r)

  let fulfill r x = Eio.Promise.resolve r x

  let await p = Eio.Promise.await p
end

module Schema = struct
  type ('ctx, 'a) typ = ('ctx, 'a) Graphql.Schema.typ
  type ('ctx, 'args, 'a) field = ('ctx, 'args, 'a) Graphql.Schema.field

  let schema = Graphql.Schema.schema
  let obj = Graphql.Schema.obj
  let field = Graphql.Schema.field
  let arg = Graphql.Schema.Arg.arg
  let string = Graphql.Schema.string
  let int = Graphql.Schema.int
  let float = Graphql.Schema.float
  let bool = Graphql.Schema.bool
  let list = Graphql.Schema.list
  let non_null = Graphql.Schema.non_null
  let arg_opt = Graphql.Schema.Arg.arg_opt
  let arg_from_enum = Graphql.Schema.Arg.arg_from_enum
  let enum = Graphql.Schema.enum
  let iso_string = Graphql.Schema.iso_string
  let ident = Graphql.Schema.ident
end

module Io = struct
  module Future = Future
  module Stream = struct
    type 'a t = 'a Eio_stream.t
    let create () = Eio_stream.create 1
    let yield s x = Eio_stream.add s x
    let close s = Eio_stream.close s
    let on_cancel _ = ()
  end
end

module Make_graphql_server = Graphql_server.Make (Io)
