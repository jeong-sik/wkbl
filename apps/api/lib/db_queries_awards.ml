(** Auto-generated from db_queries.ml split. *)

open Db_request
open Caqti_type

let upsert_award = (t2 string (t2 string (t2 string (t2 string (t2 (option string) (option string))))) ->. unit) {|
  INSERT INTO awards (season_name, category, award_type, player_name, stat_value, votes)
  VALUES (?, ?, ?, ?, ?, ?)
  ON CONFLICT (season_name, category, player_name)
  DO UPDATE SET
    award_type = EXCLUDED.award_type,
    stat_value = COALESCE(EXCLUDED.stat_value, awards.stat_value),
    votes = COALESCE(EXCLUDED.votes, awards.votes)
|}

(** Query awards by category *)


let awards_by_category = (string ->* t2 string (t2 string (t2 string (t2 (option string) (option string))))) {|
  SELECT season_name, category, player_name, stat_value, votes
  FROM awards
  WHERE category = ?
  ORDER BY season_name DESC
|}

(** Query awards by season *)


let awards_by_season = (string ->* t2 string (t2 string (t2 string (t2 (option string) (option string))))) {|
  SELECT season_name, category, player_name, stat_value, votes
  FROM awards
  WHERE season_name = ?
  ORDER BY category
|}

(** Query awards by player name (partial match) *)


let awards_by_player = (string ->* t2 string (t2 string (t2 string (t2 (option string) (option string))))) {|
  SELECT season_name, category, player_name, stat_value, votes
  FROM awards
  WHERE player_name LIKE '%' || ? || '%'
  ORDER BY season_name DESC, category
|}

(** Count total awards *)


let count_awards = (unit ->? int) {|
  SELECT COUNT(*) FROM awards
|}

(** Get distinct award winners with win counts *)


let award_leaders = (string ->* t2 string int) {|
  SELECT player_name, COUNT(*) as cnt
  FROM awards
  WHERE category = ?
  GROUP BY player_name
  ORDER BY cnt DESC
  LIMIT 20
|}

