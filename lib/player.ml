open! Core

type t =
  { mutable name : string
  ; mutable score : int
  ; mutable living : bool
  ; mutable answered_mr_question : bool
  ; mutable answered_mr_question_wrong : bool
  ; mutable color : int
  }
[@@deriving compare, equal, sexp_of]

(* this is the default player *)
let default_create_single_player () =
  { name = ""
  ; score = 0
  ; living = true
  ; answered_mr_question = false
  ; answered_mr_question_wrong = false
  ; color = Color.random ()
  }
;;

(* creates a player with the name *)
let name_create_single_player str =
  { name = str
  ; score = 0
  ; living = true
  ; answered_mr_question = false
  ; answered_mr_question_wrong = false
  ; color = Color.random ()
  }
;;

(* creates a list of 4 default players *)
let create_multi_players () =
  [ default_create_single_player ()
  ; default_create_single_player ()
  ; default_create_single_player ()
  ; default_create_single_player ()
  ]
;;

let update_name t str =
  if String.equal t.name ""
  then t.name <- str
  else failwith "This player already has a name!"
;;

let player_loses t =
  t.living <- false;
  t.color <- Color.dead_gray
;;

(* expect tests *)
let%expect_test "test player initialization" =
  print_endline "hello";
  [%expect {| hello |}]
;;
