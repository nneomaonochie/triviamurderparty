open! Core

module Game_kind = struct
  type t =
    | Trivia
    | Math_mayhem of Player.t list
    | Decisions of Player.t list
    | Button_mash of Player.t list
  [@@deriving compare, equal, sexp_of]
end

type t =
  { (* players should stay the same, but a player being dead or alive should
       be mutable *)
    player_list : Player.t list
  ; mutable game_type : Game_kind.t (* mutable *)
  ; mutable game_state : Game_state.t (* mutable *)
  }
[@@deriving sexp_of, compare]

(* creates the first instance of the game that shows up after the beginning
   screen *)
let create (players : Player.t list) : t =
  { player_list = players; game_type = Trivia; game_state = Ongoing }
;;

(* try-catch for when not 4 is created *)

(* expect tests for game creation *)
let%expect_test "test game creation" =
  let game =
    create
      (Player.create_multi_players [ "anoushka"; "nneoma"; "hao"; "ben" ])
  in
  print_s [%message "" (game : t)];
  [%expect
    {|
    (game
     ((player_list
       (((name anoushka) (score 0) (living true))
        ((name nneoma) (score 0) (living true))
        ((name hao) (score 0) (living true))
        ((name ben) (score 0) (living true))))
      (game_type Trivia) (game_state Ongoing))) |}]
;;

let%expect_test "changing player state" =
  let gg =
    create
      (Player.create_multi_players [ "anoushka"; "nneoma"; "hao"; "ben" ])
  in
  let hao = List.nth_exn gg.player_list 2 in
  hao.living <- false;
  gg.game_state <- Game_over;
  print_s [%message "" (gg : t)];
  [%expect
    {|
  (gg
   ((player_list
     (((name anoushka) (score 0) (living true))
      ((name nneoma) (score 0) (living true))
      ((name hao) (score 0) (living false))
      ((name ben) (score 0) (living true))))
    (game_type Trivia) (game_state Game_over))) |}]
;;

(*[%expect {| (player (name hao) (score 0) (living false))|}]; print_s
  [%message "" (game : t)]; [%expect {| (game ((player_list (((name anoushka)
  (score 0) (living true)) ((name nneoma) (score 0) (living true)) ((name
  hao) (score 0) (living false)) ((name ben) (score 0) (living true))))
  (game_type Trivia) (game_state Ongoing))) |}];*)
