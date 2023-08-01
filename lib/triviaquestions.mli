open! Core
open! Yojson

val get_data_from_file : string -> Basic.t
val get_question_array : string -> Basic.t list
val pick_random_question : unit -> Question.Question.t
val print_random_question : unit -> unit
