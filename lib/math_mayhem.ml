open! Core
open! Async

(* its own graphics? *)

type t = { participants : (Socket.Address.Inet.t * Player.t) list }
[@@deriving sexp_of, compare]

(* sends a Math_Mayhem minigame with a list of players that are participating
   in the minigame *)
let initialize (players : (Socket.Address.Inet.t * Player.t) list) =
  { participants = players }
;;

(* runs the game *)
let run_game t =
  (* i need to send a response with being prompted... maybe when they send
     DONE, i start sending questions *)

  (* this will probably be in a loop *)
  let operator = " + " in
  (* math_mayhem.graphics t *)
  let first = Random.int 41 - 20 in
  (* random number from -20 to 20 *)
  let second = Random.int 41 - 20 in
  let correct_answer = first + second in
  (* didn't figure out the subtract thing yet *)
  let question_string =
    Int.to_string first ^ operator ^ Int.to_string second
  in
  (* the question we will show to the client *)

  (* this will probably be the end of the loop ß*)

  (* list of questions to pull *)
  (* display questions to player - this will utilize Protocol.Response.t *)
  (* get player responses - each correct answer is +25 to score *)
  (* wrong answers - in OG game they get stunned for X secs, may just
     implement a $$ deduction instead *)
  (* keep track of score - lowest score dies *)

  (* the case where only one player is sent - get other players to play or
     that they must reach 20 correct Q's *)
  ()
;;
