open! Core

type t =
  | Player_Initializion
  | Ongoing
  | Game_over
[@@deriving compare, equal, sexp_of]
