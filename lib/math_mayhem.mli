open! Core
open! Async

type t =
  { mutable correct_answers : (string, int) Base.Hashtbl.t
  ; mutable current_points : (string, int) Base.Hashtbl.t
  ; mutable player_positions :
      ((Socket.Address.Inet.t * Player.t) * int) list
  }
[@@deriving sexp_of]

val create : unit -> t
val get_questions : unit -> string * int
