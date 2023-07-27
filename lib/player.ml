open! Core

type t =
  { name : string
  ; mutable score : int
  ; mutable living : bool
  }
[@@deriving compare, equal, sexp_of]

(* expect tests *)
let%expect_test "test player initialization" =
  print_endline "hello";
  [%expect {| hello |}]
;;
