open! Core
open! Async

module Game_kind : sig
  type t =
    | Leaderboard
    | Trivia of Question.t
    | Math_mayhem
    | Decisions
    (* if false, it's in password creation mode; if true its in password
       guessing mode*)
    | Password_pain of bool
  [@@deriving compare, sexp_of]
end

type t =
  { (* players should stay the same, but a player being dead or alive should
       be mutable *)
    mutable player_list :
      (Socket.Address.Inet.t * Player.t) list (* a Client * Player touple *)
  ; mutable game_type : Game_kind.t
  ; mutable game_state : Game_state.t
  ; mutable questions_asked : int
  }
[@@deriving sexp_of, compare]

val get_ip_address : Socket.Address.Inet.t -> string
val set_up_players : Socket.Address.Inet.t -> string -> t -> t * bool
val ask_question : t -> unit
val create : unit -> t
