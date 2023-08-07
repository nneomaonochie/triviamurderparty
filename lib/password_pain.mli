open! Core
open! Async

type t =
  { mutable active_participants :
      (Socket.Address.Inet.t * Player.t * string * string * int) list
  ; mutable safe_players : (Socket.Address.Inet.t * Player.t) list
  }
[@@deriving sexp_of]

val create : unit -> t
val update_password : t -> query:string -> unit
val check_guess : t -> string -> Game.t -> unit
