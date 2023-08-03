open! Core

(* the player module is to keep track of the players and update their scores
   throughout the game *)
type t =
  { mutable name : string
  ; mutable score : int
  ; mutable living : bool
  ; mutable answered_mr_question : bool
  ; mutable answered_mr_question_wrong : bool
  ; color : int
  }
[@@deriving compare, equal, sexp_of]

(* creates a default Player.t *)
val default_create_single_player : unit -> t

(* creates a Player.t with the string as the given name *)
val name_create_single_player : string -> t

(* creates a Player.t list of default players *)
val create_multi_players : unit -> t list

(* changes a player's name so long as the previous name was an empty
   string *)
val update_name : t -> string -> unit
