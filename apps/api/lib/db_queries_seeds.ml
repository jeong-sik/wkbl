(** Auto-generated from db_queries.ml split. *)

open Db_request
open Caqti_type

let seed_legend_players = (unit ->. unit) {|
  INSERT INTO legend_players (player_name, career_years, teams, championships, mvp_count, all_star_count, career_points, career_rebounds, career_assists)
  VALUES
  ('정선민', '1998-2012', '신세계, KB스타즈, 신한은행', 9, 7, 12, 8140, 3142, 1777),
  ('박정은', '1998-2013', '삼성생명', 5, 0, 11, 6540, 2664, 1776),
  ('변연하', '1999-2016', '삼성생명, KB스타즈', 5, 3, 13, 7863, 2227, 2407),
  ('이미선', '1998-2016', '삼성생명', 6, 0, 12, 5323, 2664, 2264),
  ('신정자', '1999-2016', '국민은행, 금호생명, 신한은행', 6, 1, 11, 5970, 4502, 1753),
  ('김지윤', '1998-2013', '국민은행, 금호생명, 신세계, 하나외환', 0, 1, 12, 7020, 1867, 2733),
  ('전주원', '1998-2011', '현대, 신한은행', 7, 1, 10, 4325, 1546, 2164),
  ('김단비', '2007-Present', '신한은행, 우리은행', 6, 1, 13, 7000, 3000, 2500),
  ('박혜진', '2008-Present', '우리은행', 8, 5, 10, 5000, 2000, 1500),
  ('강이슬', '2012-Present', '하나은행, KB스타즈', 1, 0, 8, 4500, 1500, 800)
  ON CONFLICT (player_name) DO NOTHING
|}

(* Seed historical/defunct teams and All-Star/special event teams so FK updates can use them. *)


let seed_historical_teams = (unit ->. unit) {|
  INSERT INTO teams (team_code, team_name_kr, team_name_en)
  VALUES
    ('02', '금호생명', 'Kumho Life'),
    ('04', '신세계', 'Shinsegae'),
    ('06', '현대', 'Hyundai'),
    ('12', 'LG', 'LG'),
    ('13', '한화', 'Hanwha'),
    ('81', '팀 유니블', 'Team Unibble'),
    ('82', '팀 포니블', 'Team Ponibble'),
    ('83', '한국 올스타', 'Korea All-Star'),
    ('84', '일본 올스타', 'Japan All-Star'),
    ('87', '핑크스타', 'Pink Star'),
    ('88', '블루스타', 'Blue Star'),
    ('91', '남부선발', 'South Select'),
    ('92', '중부선발', 'Central Select')
  ON CONFLICT (team_code) DO NOTHING
|}

