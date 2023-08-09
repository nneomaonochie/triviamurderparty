open! Core
open! Yojson

(* Module that will store final round questions randomly selected from JSON
   file*)

type t =
  { category : string
  ; right_answers : string list
  ; wrong_answers : string list
  ; mutable correct_chars : char list
      (* whatever the right answers char corresponds with, put in this list
         to compare users input to correct chars *)
  }
[@@deriving compare, equal, sexp_of]

(* Converting JSON file into Yojson datatype*)
let get_data_from_file filename = Yojson.Basic.from_file filename

let get_question_array file_name =
  let open Yojson.Basic.Util in
  let json = get_data_from_file file_name in
  json |> to_list
;;

let pick_random_question () : t =
  let open Yojson.Basic.Util in
  let array =
    get_question_array
      "/usr/local/home/jsipuser/triviamurderparty/lib/final_round_questions.json"
  in
  let elem = List.random_element_exn array in
  let c = elem |> member "category" |> to_string in
  let wrong_answers =
    elem
    |> member "W"
    |> to_list
    |> List.map ~f:(fun elem -> Basic.to_string elem)
  in
  let right_answers =
    elem
    |> member "R"
    |> to_list
    |> List.map ~f:(fun elem -> Basic.to_string elem)
  in
  { category = c; right_answers; wrong_answers; correct_chars = [] }
;;

let print_random_question () =
  let q = pick_random_question () in
  print_s [%message "Random Question: " (q : t)]
;;

(* 

   1. separate by the bullet point 2. put stuff in bb -strike in a the W list
   3. put reats in the R list *)
