open Graphql_eio
open Domain

module Schema = Graphql_eio.Make_graphql_server.Schema

(* Define GraphQL types for existing OCaml types *)

let player_type : (unit, player_aggregate) Schema.typ = Schema.(obj "Player"
  ~fields:(fun _ -> [
    field "player_id" ~typ:(non_null string) ~args:Schema.Arg.[]
      ~resolve:(fun _ (p : player_aggregate) -> p.player_id);
    field "name" ~typ:(non_null string) ~args:Schema.Arg.[]
      ~resolve:(fun _ (p : player_aggregate) -> p.name);
    field "team_name" ~typ:(non_null string) ~args:Schema.Arg.[]
      ~resolve:(fun _ (p : player_aggregate) -> p.team_name);
    field "avg_points" ~typ:(non_null float) ~args:Schema.Arg.[]
      ~resolve:(fun _ (p : player_aggregate) -> p.avg_points);
    (* Add other fields from player_aggregate as needed *)
  ])
)

let query_type = Schema.(obj "Query" ~fields:(fun _ -> [
  field "hello" ~typ:(non_null string) ~args:Schema.Arg.[]
    ~resolve:(fun _ _ -> "Hello, OCaml GraphQL!");

  field "players" ~typ:(non_null (list (non_null player_type)))
    ~args:Schema.Arg.[
      arg "name" ~typ:(string) ~doc:"Filter players by name";
    ]
    ~resolve:(fun _ _ name ->
      let (let*) = Eio.Promise.bind in (* This needs to be Future.bind, not Promise.bind *)
      let p, r = Eio.Promise.create () in
      let players_data =
        match name with
        | Some n ->
            (* This part needs actual Eio DB interaction *)
            [{
              player_id = "test_id";
              name = n;
              team_name = "test_team";
              games_played = 0;
              total_minutes = 0.0;
              total_points = 0;
              total_rebounds = 0;
              total_assists = 0;
              total_steals = 0;
              total_blocks = 0;
              total_turnovers = 0;
              avg_points = 0.0;
              avg_margin = 0.0;
              avg_rebounds = 0.0;
              avg_assists = 0.0;
              avg_steals = 0.0;
              avg_blocks = 0.0;
              total_turnovers = 0;
              efficiency = 0.0;
              total_fg_made = 0;
              total_fg_att = 0;
              total_fg3_made = 0;
              total_fg3_att = 0;
              total_ft_made = 0;
              total_ft_att = 0;
            }]
        | None ->
            [{
              player_id = "test_id_all";
              name = "Test Player All";
              team_name = "test_team_all";
              games_played = 0;
              total_minutes = 0.0;
              total_points = 0;
              total_rebounds = 0;
              total_assists = 0;
              total_steals = 0;
              total_blocks = 0;
              total_turnovers = 0;
              avg_points = 0.0;
              avg_margin = 0.0;
              avg_rebounds = 0.0;
              avg_assists = 0.0;
              avg_steals = 0.0;
              avg_blocks = 0.0;
              total_turnovers = 0;
              efficiency = 0.0;
              total_fg_made = 0;
              total_fg_att = 0;
              total_fg3_made = 0;
              total_fg3_att = 0;
              total_ft_made = 0;
              total_ft_att = 0;
            }]
      in
      Eio.Promise.resolve r players_data;
      p
    )
]))

let schema = Schema.schema query_type