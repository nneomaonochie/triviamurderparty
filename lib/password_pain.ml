open! Core
open! Async

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
let update_password client_ip t ~query =
  (* make sure they cant RE-update it *)
  t.active_participants
    <- List.map
         t.active_participants
         ~f:(fun (c, p, real_pw, display_pw, i) ->
         if String.equal client_ip (Game.get_ip_address c)
            && String.is_empty real_pw
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
let check_guess client_ip t (guess : string) (game : Game.t) =
  (* the client string is for Every Player Mode - we dont want to guess our
     own password *)
  let updated_pp_positions =
    List.map t.active_participants ~f:(fun (c, pl, real_pw, display_pw, i) ->
      let real_p = real_pw in
      (* result is user's correct letter placements, which we will compare
         with the display password *)
      let result =
        if String.equal (Game.get_ip_address c) client_ip
        then "****"
        else compare_answers ~real_pw:real_p ~guess ~index:0 ~result:""
      in
      let new_display_pw =
        String.foldi display_pw ~init:"" ~f:(fun ind init char ->
          if Char.equal char '*'
          then init ^ Char.to_string (String.get result ind)
          else init ^ Char.to_string char)
      in
      (* if no *'s present then it has been completely guessed *)
      if not (String.contains new_display_pw '*')
      then
        List.iter game.player_list ~f:(fun (_, p) ->
          if Player.equal p pl
          then (
            p.living <- false;
            pl.living <- false));
      (* the new password is now the display pw *)
      c, pl, real_pw, new_display_pw, i)
  in
  t.active_participants <- updated_pp_positions
;;

(* if everyone gets password guess, just go straight to display losers *)

(* the reprinting of the password graphics is scuffy *)

(* make sure players cant submit non-words ec: nneo*)

(* there is a chance that the game passed hasnt updated yet... *)
