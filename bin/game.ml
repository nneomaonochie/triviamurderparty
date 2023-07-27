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