open Graphql_eio
open Domain

module Schema : Graphql_server.Schema.S with module Future := Graphql_eio.Future and type 'a Io.Stream.t = 'a Eio_stream.t
