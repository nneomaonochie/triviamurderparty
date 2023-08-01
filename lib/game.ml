open! Core
open! Async

module Game_kind = struct
  type t =
    | Trivia of Question.t
    | Math_mayhem of (Socket.Address.Inet.t * Player.t) list
    | Decisions of (Socket.Address.Inet.t * Player.t) list
    | Button_mash of (Socket.Address.Inet.t * Player.t) list
  [@@deriving compare, sexp_of]
end

(* we need to try to store the client IP address with the player its attached
   to *)

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
  print_s [%message client_str];
  client_str
;;

let set_up_players client (query : string) t : t =
  if List.length t.player_list < 4
  then
    (* we need to ensure that we have 4 unique clients *)
    if not
         (* they repeat IP adresses byt clients are differnet based off of
            TIME... *)
         (List.exists t.player_list ~f:(fun (c, _) ->
            String.equal (get_ip_address c) (get_ip_address client)))
    then
      t.player_list
        <- t.player_list @ [ client, Player.name_create_single_player query ];
  if List.length t.player_list = 4
  then
    t.game_state
      <- Ongoing (* Tmp_graphics.display_beginning_instructions t); *);
  t
;;

let ask_question (game : t) =
  match game.game_type with
  | Trivia _ ->
    game.game_type <- Trivia (Triviaquestions.pick_random_question ())
  | _ -> ()
;;

(* try-catch for when not 4 is created *)

(* expect tests for game creation let%expect_test "test game creation" = let
   game = create (Player.create_multi_players [ "anoushka"; "nneoma"; "hao";
   "ben" ]) in print_s [%message "" (game : t)]; [%expect {| (game
   ((player_list (((name anoushka) (score 0) (living true)) ((name nneoma)
   (score 0) (living true)) ((name hao) (score 0) (living true)) ((name ben)
   (score 0) (living true)))) (game_type Trivia) (game_state Ongoing))) |}]
   ;;

   let%expect_test "changing player state" = let gg = create
   (Player.create_multi_players [ "anoushka"; "nneoma"; "hao"; "ben" ]) in
   let hao = List.nth_exn gg.player_list 2 in hao.living <- false;
   gg.game_state <- Game_over; print_s [%message "" (gg : t)]; [%expect {|
   (gg ((player_list (((name anoushka) (score 0) (living true)) ((name
   nneoma) (score 0) (living true)) ((name hao) (score 0) (living false))
   ((name ben) (score 0) (living true)))) (game_type Trivia) (game_state
   Game_over))) |}] ;;

   let%expect_test "checking it only allows 4 players" = let gme = create
   (Player.create_multi_players []) in print_s [%message "" (gme : t)];
   [%expect {| (gme ((player_list (()) (game_type Trivia) (game_state
   Game_over))) |}] ;; *)
