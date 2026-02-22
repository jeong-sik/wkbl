open Eio.Std

module Future : Graphql_server.Io.Future.S with type 'a t = 'a Eio.Promise.t

module Schema : Graphql.Schema.S

module Io : Graphql_server.Io.S with module Future := Future and type 'a Stream.t = 'a Eio_stream.t

module Make_graphql_server : Graphql_server.Api.S with type 'a Io.Future.t = 'a Eio.Promise.t and type 'a Io.Stream.t = 'a Eio_stream.t
