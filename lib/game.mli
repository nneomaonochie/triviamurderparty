open! Core
open! Async

module Game_kind : sig
  type t =
    | Leaderboard
    | Trivia of Question.t
    | Math_mayhem
    (* if minigames are false, they are set to their set up mode, where other
       players set up the game_state*)
    (* if set to true, then players who must guess / pick will do so *)
    | Chalices of bool
    | Password_pain of bool
  [@@deriving compare, sexp_of]
end

type t =
  { mutable player_list : (Socket.Address.Inet.t * Player.t) list
  ; mutable game_type : Game_kind.t
  ; mutable game_state : Game_state.t
  ; mutable questions_asked : int
  }
[@@deriving sexp_of, compare]

val get_ip_address : Socket.Address.Inet.t -> string
val set_up_players : Socket.Address.Inet.t -> string -> t -> t * bool
val ask_question : t -> unit
val create : unit -> t
val get_players_by_score : t -> (Socket.Address.Inet.t * Player.t) list
