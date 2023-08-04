open! Core
open! Async

type t =
  { mutable correct_answers : (string, int) Base.Hashtbl.t
  ; mutable current_points : (string, int) Base.Hashtbl.t
  ; mutable player_positions :
      ((Socket.Address.Inet.t * Player.t) * int) list
  }
[@@deriving sexp_of]

let create () : t =
  { correct_answers = Hashtbl.create (module String)
  ; current_points = Hashtbl.create (module String)
  ; player_positions = []
  }
;;

let get_questions () =
  let operator_str = if Random.int 2 % 2 = 0 then " + " else " - " in
  (* random number from -20 to 20 *)
  let first = Random.int 41 - 20 in
  let second = Random.int 41 - 20 in
  let correct_answer =
    first
    +
    if String.equal operator_str " + "
    then second
    else
      second * -1 (* if the operand is subtract, then we are subtracting *)
  in
  let question_string =
    Int.to_string first ^ operator_str ^ Int.to_string second ^ " = ? "
  in
  question_string, correct_answer
;;

(* 6. [do last] the case where only one player is sent - get other players to
   play or that they must reach 20 correct Q's *)
