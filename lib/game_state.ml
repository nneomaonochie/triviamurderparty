open! Core

type t =
  | Ongoing
  | Game_over
[@@deriving compare, equal, sexp_of]
