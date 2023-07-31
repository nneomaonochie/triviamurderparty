open! Core

(* the player module is to keep track of the players and update their scores
   throughout the game *)
type t =
  { mutable name : string
  ; mutable score : int
  ; mutable living : bool
  }
[@@deriving compare, equal, sexp_of]

(* creates a Player using a given string *)
val create_single_player : string -> t

(* creates a Player.t list using a string of player names *)
val create_multi_players : string list -> t list
