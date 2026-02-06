(** Season catalog (WKBL schedule API codes).

    Source of truth for mapping `season_code` -> human readable name.
    Codes are used by WKBL main schedule endpoint: `/game/sch/inc_list_1_new.asp`. *)

let all : (string * string) list = [
  (* Modern era: Regular seasons (Oct-Mar) *)
  ("046", "2025-2026");
  ("045", "2024-2025");
  ("044", "2023-2024");
  ("043", "2022-2023");
  ("042", "2021-2022");
  ("041", "2020-2021");
  ("040", "2019-2020");
  ("039", "2018-2019");
  ("038", "2017-2018");
  ("037", "2016-2017");
  ("036", "2015-2016");
  ("035", "2014-2015");
  ("034", "2013-2014");
  ("033", "2012-2013");
  ("032", "2011-2012");
  ("031", "2010-2011");
  (* Transition era: Mixed formats *)
  ("030", "2010퓨처스");
  ("029", "2009-2010");
  ("028", "2009퓨처스");
  ("027", "2008-2009");
  ("026", "2008퓨처스");
  ("025", "2007-2008");
  ("024", "2007퓨처스");
  ("023", "2007겨울");
  ("022", "2006퓨처스");
  ("021", "2006여름");
  ("020", "2006겨울");
  ("019", "2005여름");
  ("018", "2005퓨처스");
  ("017", "2005겨울");
  ("016", "2004퓨처스");
  ("015", "2004겨울");
  (* Early era: Summer/Winter leagues *)
  ("013", "2003여름");
  ("012", "2003겨울");
  ("011", "2002여름");
  ("009", "2002겨울");
  ("008", "2001여름");
  ("007", "2001겨울");
  ("006", "2000여름");
  ("005", "2000겨울");
  ("003", "1999여름");
  ("002", "1999겨울");
  ("001", "1998여름");
]

let name_of_code code =
  List.assoc_opt code all
  |> Option.value ~default:(Printf.sprintf "Unknown-%s" code)

let is_regular_season_name name =
  String.length name >= 9 && name.[4] = '-'

let regular : (string * string) list =
  all |> List.filter (fun (_code, name) -> is_regular_season_name name)

