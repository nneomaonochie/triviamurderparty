open! Core
open! Yojson
open! Async

(* Module that will store final round questions randomly selected from JSON
   file*)

type t =
  { mutable category : string
  ; mutable right_answers : string list
  ; mutable wrong_answers : string list
  ; mutable char_placements : bool list
      (* in order of Q, W, E - if char is a correct answer, it will be
         true *)
  ; mutable final_players :
      (Socket.Address.Inet.t * Player.t * int * int * int) list
  ; mutable player_guesses :
      (Socket.Address.Inet.t * Player.t * bool list * bool) list
      (* the last bool checks if user has answered *)
  }
[@@deriving compare, sexp_of]

(* Converting JSON file into Yojson datatype*)
let get_data_from_file filename = Yojson.Basic.from_file filename

let get_question_array file_name =
  let open Yojson.Basic.Util in
  let json = get_data_from_file file_name in
  json |> to_list
;;

(* this picks a random category from the final_round_questions.json *)
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
  { category = c
  ; right_answers
  ; wrong_answers
  ; char_placements = []
  ; final_players = []
  ; player_guesses = []
  }
;;

(* this is for debugging purposes *)
let print_random_question () =
  let q = pick_random_question () in
  print_s [%message "Random Question: " (q : t)]
;;
