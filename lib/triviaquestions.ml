(* Module that will store questions that are randomly selected from JSON
   file*)
open! Core
open! Yojson

(* Converting JSON file into Yojson datatype*)
let get_data_from_file filename = Yojson.Basic.from_file filename

let get_question_array file_name =
  let open Yojson.Basic.Util in
  let json = get_data_from_file file_name in
  json |> to_list
;;

(* randomly pulls a questions from the trivia.json and assigns it to key
   letters*)
let pick_random_question () : Question.t =
  let open Yojson.Basic.Util in
  let array =
    get_question_array
      "/usr/local/home/jsipuser/triviamurderparty/lib/trivia.json"
  in
  let array_size = List.length array in
  let random_idx = Random.int array_size in
  let elem = List.nth_exn array random_idx in
  let q = elem |> member "question" |> to_string in
  let one = elem |> member "Q" |> to_string in
  let two = elem |> member "W" |> to_string in
  let three = elem |> member "E" |> to_string in
  let four = elem |> member "R" |> to_string in
  let ans_choices =
    []
    @ [ String.concat [ "Q: "; one ] ]
    @ [ String.concat [ "W: "; two ] ]
    @ [ String.concat [ "E: "; three ] ]
    @ [ String.concat [ "R: "; four ] ]
  in
  let c_ans = elem |> member "answer" |> to_string in
  { question = q; answer_choices = ans_choices; correct_answer = c_ans }
;;

(* used for testing, code that picks a random questions and prints it to the
   terminal *)
let print_random_question () =
  let q = pick_random_question () in
  print_s [%message "Random Question: " (q : Question.t)]
;;
