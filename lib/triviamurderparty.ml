open! Core
open Async

let main : unit -> unit Deferred.t =
 fun () ->
  Core.print_endline "Hello world!";
  return ()
;;

let command =
  Command.async
    ~summary:"Start server for example [starter_template]"
    (let%map_open.Command () = return () in
     fun () -> main ())
;;