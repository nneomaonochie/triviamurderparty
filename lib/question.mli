open! Core

type t =
  { question : string
  ; answer_choices : string list
  ; correct_answer : string
  }
[@@deriving compare, equal, sexp_of]
