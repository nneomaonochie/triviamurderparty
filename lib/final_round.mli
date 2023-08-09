open! Core
open! Yojson
open! Async

type t =
  { mutable category : string
  ; mutable right_answers : string list
  ; mutable wrong_answers : string list
  ; mutable char_placements : bool list
  ; mutable final_players :
      (Socket.Address.Inet.t * Player.t * int * int * int) list
  }
[@@deriving compare, sexp_of]

val get_data_from_file : string -> Basic.t
val get_question_array : string -> Basic.t list
val pick_random_question : unit -> t
val print_random_question : unit -> unit
