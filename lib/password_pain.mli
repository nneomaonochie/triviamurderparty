open! Core
open! Async

type t =
  { mutable active_participants :
      (Socket.Address.Inet.t * Player.t * string * string * int) list
  ; mutable safe_players : (Socket.Address.Inet.t * Player.t) list
  ; mutable all_passwords_guessed : bool
  }
[@@deriving sexp_of]

val create : unit -> t
val update_password : string -> t -> query:string -> unit
val check_guess : string -> t -> string -> Game.t -> unit
