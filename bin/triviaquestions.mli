open! Core
open! Yojson

module Question : sig
  type t =
    { question : string
    ; answer_choices : string list
    ; correct_answer : string
    }
end

val get_data_from_file : string -> Basic.t
val get_question_array : string -> Basic.t list
val pick_random_question : unit -> Question.t
val print_random_question : unit -> unit
