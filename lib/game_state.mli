open! Core

(* this keeps track of whether the game has ended or not *)
type t =
  | Player_Initializion
  | Ongoing
  | Game_over
[@@deriving compare, equal, sexp_of]
