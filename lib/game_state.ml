open! Core

type t =
  | Player_Initializion
  | Ongoing
  | Final_round
  | Game_over
[@@deriving compare, equal, sexp_of]
