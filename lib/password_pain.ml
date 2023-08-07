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
  { mutable active_participants : (Socket.Address.Inet.t * Player.t) list
  ; mutable safe_players : (Socket.Address.Inet.t * Player.t) list
      (* this is a list of players and the passwords they choice *)
  ; mutable player_passwords_positions :
      (Socket.Address.Inet.t * Player.t * string * string * int) list
      (* the FIRST string is the real password, and the SECOND string is what
         we display *)
  }
[@@deriving sexp_of]

let create () =
  { active_participants = []
  ; safe_players = []
  ; player_passwords_positions = []
  }
;;

let update_password t ~client_ip ~query =
  (* make sure they cant RE-update it *)
  let player_positions = t.player_passwords_positions in
  (* inserts the new password into the record *)
  let player_positions =
    List.map player_positions ~f:(fun (c, p, real_pw, display_pw, i) ->
      if String.equal client_ip (Game.get_ip_address c)
      then c, p, query, "****", i
      else c, p, real_pw, display_pw, i)
  in
  t.player_passwords_positions <- player_positions
;;

(* recursively checks which chars in a string are equivalent *)
let rec compare_answers ~real_pw ~guess ~index ~result =
  if index = 4
  then result
  else (
    let result =
      if Char.equal (String.get real_pw index) (String.get guess index)
      then result ^ Char.to_string (String.get real_pw index)
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
  List.iter t.player_passwords_positions ~f:(fun (_, _, real_pw, _, _) ->
    let result = compare_answers ~real_pw ~guess ~index:0 ~result:"" in
    (* eventually change to change to display_pq*)
    print_s [%message result]);
  ""
;;
