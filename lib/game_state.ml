open! Core

type t =
  (* should we make a Beginning module? *)
  | Ongoing
  | Game_over
[@@deriving compare, equal, sexp_of]