(** Player Similarity Calculation **)

open Domain

type similarity_score = {
  player_id: string;
  player_name: string;
  team_name: string;
  score: float; (** 0.0 to 1.0, 1.0 being most similar *)
}

(** Calculate Euclidean distance between two players based on normalized stats *)
let euclidean_distance (p1: player_aggregate) (p2: player_aggregate) =
  let d1 = p1.avg_points -. p2.avg_points in
  let d2 = p1.avg_rebounds -. p2.avg_rebounds in
  let d3 = p1.avg_assists -. p2.avg_assists in
  let d4 = p1.avg_steals -. p2.avg_steals in
  let d5 = p1.avg_blocks -. p2.avg_blocks in
  let d6 = p1.efficiency -. p2.efficiency in
  
  sqrt (
    (d1 *. d1) +. 
    (d2 *. d2) +. 
    (d3 *. d3) +. 
    (d4 *. d4) +. 
    (d5 *. d5) +. 
    (d6 *. d6)
  )

(** Convert distance to similarity score [0, 1] *)
let distance_to_similarity dist =
  1.0 /. (1.0 +. dist)

let calculate_similar_players (target: Domain.player_aggregate) (players: Domain.player_aggregate list) ~limit : similarity_score list =
  let other_players = List.filter (fun (p: Domain.player_aggregate) -> p.player_id <> target.player_id) players in
  let scores = List.map (fun (p: Domain.player_aggregate) ->
      let dist = euclidean_distance target p in
      {
        player_id = p.player_id;
        player_name = p.name;
        team_name = p.team_name;
        score = distance_to_similarity dist;
      }
    ) other_players in
  let sorted_scores = List.sort (fun s1 s2 -> Float.compare s2.score s1.score) scores in
  let rec take n = function
    | [] -> []
    | _ when n <= 0 -> []
    | x :: xs -> x :: take (n - 1) xs
  in
  take limit sorted_scores