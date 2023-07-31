open! Core

type t =
  { mutable name : string
  ; mutable score : int
  ; mutable living : bool
  (* IP adress *)
  }
[@@deriving compare, equal, sexp_of]

let create_single_player player_name : t =
  { name = player_name; score = 0; living = true }
;;

let create_multi_players (multi_players : string list) : t list =
  List.map multi_players ~f:create_single_player
;;

let update_name = 

(* expect tests *)
let%expect_test "test player initialization" =
  print_endline "hello";
  [%expect {| hello |}]
;;
