open! Core
open! Async

(* 

   Players that lost will have to create a 4 letter pass word [should we
   ensure its a real word or go by honor system ]

   Players that are safe need to guess - when they get certain letters they
   will be revealed

   Any password that is completely guessed will have the player that
   submitted it DIE

   LOGIC: game participants safe_players display_name (Password Pain)
   display_instructions (players participating, submit a 4 letter word) empty
   array with size of participants -> when its full, transition to
   next_instructions next_instructions (SAFE players [if all 4 dead, then
   every_player_mode] guess the password ) queries come in - if from
   SAFE_PLAYER list, then see which letters -> every letter revealed if the
   individual letter is right (like hangman) *)

(* make an every_plyaer_playing mode - whoever is guessed the MOST dies *)

type t =
  { mutable active_participants :
      (Socket.Address.Inet.t * Player.t * string * string * int) list
      (* the FIRST string is the real password, and the SECOND string is what
         we display *)
  ; mutable safe_players : (Socket.Address.Inet.t * Player.t) list
  }
[@@deriving sexp_of]

let create () = { active_participants = []; safe_players = [] }

(* initializes a user's password *)
let update_password t ~query =
  (* make sure they cant RE-update it *)
  t.active_participants
    <- List.map
         t.active_participants
         ~f:(fun (c, p, real_pw, display_pw, i) ->
         if String.is_empty real_pw
         then c, p, query, "****", i
         else c, p, real_pw, display_pw, i)
;;

(* recursively checks which chars in a string are equivalent *)
let rec compare_answers ~real_pw ~guess ~index ~result =
  if index = 4
  then result
  else (
    let result =
      if Char.equal (String.get real_pw 0) (String.get guess 0)
      then result ^ Char.to_string (String.get real_pw 0)
      else result ^ "*"
    in
    let stop_index = String.length real_pw in
    let new_real_pw =
      if String.length real_pw = 1
      then ""
      else String.slice real_pw 1 stop_index
    in
    let new_guess =
      if String.length guess = 1 then "" else String.slice guess 1 stop_index
    in
    compare_answers
      ~real_pw:new_real_pw
      ~guess:new_guess
      ~index:(index + 1)
      ~result)
;;

(* takes in a guess as a string and finds similarites with passwords *)
let check_guess t (guess : string) =
  let updated_pp_positions =
    List.map t.active_participants ~f:(fun (c, pl, real_pw, display_pw, i) ->
      let real_p = real_pw in
      (* result is user's correct letter placements, which we will compare
         with the display password *)
      let result =
        compare_answers ~real_pw:real_p ~guess ~index:0 ~result:""
      in
      let new_display_pw =
        String.foldi display_pw ~init:"" ~f:(fun ind init char ->
          if Char.equal char '*'
          then init ^ Char.to_string (String.get result ind)
          else init ^ Char.to_string char)
      in
      (* if no *'s present then it has been completely guessed *)
      if not (String.contains new_display_pw '*') then pl.living <- false;
      print_s [%message new_display_pw];
      (* the new password is now the display pw *)
      c, pl, real_pw, new_display_pw, i)
  in
  t.active_participants <- updated_pp_positions
;;

(* for the timer, maybe do it where the different ratio of safe to unsafe
   yields a different amount of time *)

(* if everyone gets password guess, just go straight to display losers *)

(* the reprinting of the password graphics is scuffy *)

(* make an every player mode*)

(* make sure players cant submit non-words ec: nneo*)
