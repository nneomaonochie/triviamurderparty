open! Core

(* the player module is to keep track of the players and update 
   their scores throughout the game *)
type t =
  { name : string
  ; mutable score : int
  ; mutable living : bool
  }
[@@deriving compare, equal, sexp_of]