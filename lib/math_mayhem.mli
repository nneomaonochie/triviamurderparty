open! Core
open! Async

(* this is the file for the Math Mayhem mini game, which is a series of basic
   arithmetic sequences - the player with the worst score dies *)
type t = { participants : (Socket.Address.Inet.t * Player.t) list }
[@@deriving sexp_of, compare]

val initialize : (Socket.Address.Inet.t * Player.t) list -> t
val run_game : t -> unit
