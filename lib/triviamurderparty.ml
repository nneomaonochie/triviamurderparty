open! Core
open Async

let main : unit -> unit Deferred.t =
 fun () ->
  Triviaquestions.print_random_question ();
  return ()
;;

let command =
  (* Command.group ~summary:"TMP server" [ "tmp-server", Tmp_server.command
     ] *)
  Command.async
    ~summary:"Start server for example [starter_template]"
    (let%map_open.Command () = return () in
     fun () -> main ())
;;
