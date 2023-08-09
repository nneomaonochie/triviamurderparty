open! Core
open! Yojson

type t =
  { category : string
  ; right_answers : string list
  ; wrong_answers : string list
  ; mutable correct_chars : char list
  }
[@@deriving compare, equal, sexp_of]

val get_data_from_file : string -> Basic.t
val get_question_array : string -> Basic.t list
val pick_random_question : unit -> t
val print_random_question : unit -> unit
