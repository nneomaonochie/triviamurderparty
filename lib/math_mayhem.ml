open! Core
open! Async

type t = { participants : (Socket.Address.Inet.t * Player.t) list }
[@@deriving sexp_of, compare]

(* sends a Math_Mayhem minigame with a list of players that are participating
   in the minigame *)
let initialize (players : (Socket.Address.Inet.t * Player.t) list) =
  { participants = players }
;;

(* 1. i need timer to stop the game... *)
(*use an ivar with and check after X seconds -> when it's full, stop, change
  the game (maybe make a winner variant, whatever ) *)

let run_game t =
  (* math_mayhem.graphics t *)
  Tmp_graphics.create_math_mayhem_graphics t.participants;
  (* this will probably be in a loop *)
  let operator = if Random.int 2 % 2 = 0 then " + " else " - " in
  let first = Random.int 41 - 20 in
  (* random number from -20 to 20 *)
  let second = Random.int 41 - 20 in
  let correct_answer =
    first
    +
    if String.equal operator " + "
    then second
    else
      second * -1 (* if the operand is subtract, then we are subtracting *)
  in
  let question_string =
    (* the question we will show to the client *)
    Int.to_string first ^ operator ^ Int.to_string second
  in
  (* this will probably be the end of the loop *)

  (* 3. display question to player - this will utilize Protocol.Response.t *)
  (* Graphics => Graphics 4 quadrants for each players each time i return a
     question stirng -> if they get get correct answer, new string show
     everything a player need to look at *)

  (* 4. get player responses - each correct answer is +25 to score *)
  (* wrong answers - in OG game they get stunned for X secs, may just
     implement a $$ deduction instead *)
  (* 5. keep track of score - lowest score dies *)

  (* 6. [do last] the case where only one player is sent - get other players
     to play or that they must reach 20 correct Q's *)
  ()
;;
