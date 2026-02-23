let () =
  let db_url = "postgresql://postgres.efgbkvmwwefqjxeugktf:dRp7We9L61Ur1sg@aws-1-ap-southeast-1.pooler.supabase.com:6543/postgres" in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match Wkbl.Db.init_pool ~sw ~stdenv:(env :> Caqti_eio.stdenv) db_url with
  | Error e -> Printf.eprintf "Init error: %s\n" (Wkbl.Db.show_db_error e)
  | Ok () ->
      match Wkbl.Db.ensure_schema () with
      | Ok () -> Printf.printf "Schema OK\n"
      | Error e -> Printf.eprintf "Schema error: %s\n" (Wkbl.Db.show_db_error e)
