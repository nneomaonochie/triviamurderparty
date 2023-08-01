open! Core
open! Async

module Game_kind : sig
  type t =
    | Trivia of Question.t
    (* Player.t list is the list of player(s) who will be participating in
       that minigame *)
    (* this list WILL be revised when the minigames have been actually
       created *)
    | Math_mayhem of Player.t list
    | Decisions of Player.t list
    | Button_mash of Player.t list
  [@@deriving compare, equal, sexp_of]
end

type t =
  { (* players should stay the same, but a player being dead or alive should
       be mutable *)
    mutable player_list :
      (Socket.Address.Inet.t * Player.t) list (* a Client * Player touple *)
  ; mutable game_type : Game_kind.t
  ; mutable game_state : Game_state.t
  }
[@@deriving sexp_of, compare]

val create : unit -> t
