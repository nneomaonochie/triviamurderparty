open! Core

(* the player module is to keep track of the players and update their scores
   throughout the game *)
type t =
  { mutable name : string
  ; mutable score : int
  ; mutable living : bool
  ; mutable answered_mr_question : bool
  ; mutable answered_mr_question_wrong : bool
  ; mutable color : int
  }
[@@deriving compare, equal, sexp_of]

(* creates a Player.t with the string as the given name *)
val name_create_single_player : string -> t
val player_loses : t -> unit
