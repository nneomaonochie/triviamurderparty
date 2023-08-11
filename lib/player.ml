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

(* if a player losers a minigame, we adjust that player *)
let player_loses t =
  t.living <- false;
  t.color <- Color.dead_gray
;;
