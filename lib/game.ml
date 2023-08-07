open! Core
open! Async

module Game_kind = struct
  type t =
    | Leaderboard
    | Trivia of Question.t
    | Math_mayhem
    | Decisions
    | Password_pain of bool
  [@@deriving compare, sexp_of]
end

type t =
  { mutable player_list : (Socket.Address.Inet.t * Player.t) list
  ; mutable game_type : Game_kind.t
  ; mutable game_state : Game_state.t
  }
[@@deriving sexp_of, compare]

let create () : t =
  { player_list = []
  ; game_type =
      Trivia { question = ""; answer_choices = []; correct_answer = "" }
  ; game_state = Player_Initializion
  }
;;

(* this parses the IP address from a client *)
let get_ip_address client : string =
  let colon_index =
    String.index_exn (Socket.Address.Inet.to_string client) ':'
  in
  (* this gives the IP address without the extra bits at *)
  let client_str =
    String.slice (Socket.Address.Inet.to_string client) 0 colon_index
  in
  client_str
;;

let ask_question (game : t) =
  match game.game_type with
  | Trivia _ ->
    game.game_type <- Trivia (Triviaquestions.pick_random_question ())
  | _ -> ()
;;

let set_up_players client (query : string) t : t * bool =
  (* we need to ensure that we have 4 unique clients *)
  if List.length t.player_list < 1
  then
    if not
         (* they repeat IP adresses byt clients are differnet based off of
            TIME... *)
         (List.exists t.player_list ~f:(fun (c, _) ->
            String.equal (get_ip_address c) (get_ip_address client)))
    then
      t.player_list
        <- t.player_list @ [ client, Player.name_create_single_player query ];
  if List.length t.player_list = 1
  then (
    t.game_state <- Ongoing;
    ask_question t);
  print_s [%message "" (t : t)];
  t, match t.game_state with Ongoing -> true | _ -> false
;;

let run_math_mayhem players t =
  Tmp_graphics.start_math_mayhem_intro ();
  let span = Time_ns.Span.of_sec 7.0 in
  Clock_ns.run_after
    span
    (fun () -> Tmp_graphics.initialize_math_mayhem_graphics players t)
    ()
;;

let run_password_pain ~participants ~safe_players =
  Tmp_graphics.start_pp_intro ~participants ~safe_players
;;

let pick_minigame ~participants ~safe_players (t : Game.t) =
  let minigames : Game_kind.t list = [ Math_mayhem; Password_pain false ] in
  t.game_type <- List.random_element_exn minigames;
  match t.game_type with
  | Math_mayhem -> run_math_mayhem participants t
  | Password_pain false -> run_password_pain ~participants ~safe_players t
;;
