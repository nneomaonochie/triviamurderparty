open! Core
open Async

let command =
  Command.group ~summary:"TMP server" [ "tmp-server", Tmp_server.command ]
;;
(* Command.async ~summary:"Start server for example [starter_template]"
   (let%map_open.Command () = return () in fun () -> main ()) *)
