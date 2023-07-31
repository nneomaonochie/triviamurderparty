open! Core

type t =
  { mutable name : string
  ; mutable score : int
  ; mutable living : bool
  }
[@@deriving compare, equal, sexp_of]

(* this is the default player *)
let create_single_player () = { name = ""; score = 0; living = true }

(* creates a list of 4 default players *)
let create_multi_players () =
  [ create_single_player ()
  ; create_single_player ()
  ; create_single_player ()
  ; create_single_player ()
  ]
;;

let update_name t str =
  if String.equal t.name ""
  then t.name <- str
  else failwith "This player already has a name!"
;;

(* expect tests *)
let%expect_test "test player initialization" =
  print_endline "hello";
  [%expect {| hello |}]
;;
