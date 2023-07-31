(*open! Core let question content : string = let open Soup in *)
open! Core
open! Yojson

(* Module that will store questions that are randomly selected from JSON
   file*)
module Question = struct
  type t =
    { question : string
    ; answer_choices : string list
    ; correct_answer : string
    }
  [@@deriving compare, equal, sexp_of]
end

(* Converting JSON file into Yojson datatype*)
let get_data_from_file filename = Yojson.Basic.from_file filename

let get_question_array file_name =
  let open Yojson.Basic.Util in
  let json = get_data_from_file file_name in
  json |> to_list
;;

let pick_random_question () : Question.t =
  let open Yojson.Basic.Util in
  let array =
    get_question_array
      "/usr/local/home/jsipuser/triviamurder/bin/trivia.json"
  in
  let array_size = List.length array in
  let random_idx = Random.int array_size in
  let elem = List.nth_exn array random_idx in
  let q = elem |> member "question" |> to_string in
  let one = elem |> member "A" |> to_string in
  let two = elem |> member "B" |> to_string in
  let three = elem |> member "C" |> to_string in
  let four = elem |> member "D" |> to_string in
  let ans_choices =
    []
    @ [ String.concat [ "A: "; one ] ]
    @ [ String.concat [ "B: "; two ] ]
    @ [ String.concat [ "C: "; three ] ]
    @ [ String.concat [ "D: "; four ] ]
  in
  let c_ans = elem |> member "answer" |> to_string in
  { question = q; answer_choices = ans_choices; correct_answer = c_ans }
;;

let print_random_question () =
  let q = pick_random_question () in
  print_s [%message "Random Question: " (q : Question.t)]
;;
