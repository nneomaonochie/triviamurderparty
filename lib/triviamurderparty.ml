open! Core
open Async

let command =
  Command.group ~summary:"TMP server" [ "tmp-server", Tmp_server.command ]
;;
