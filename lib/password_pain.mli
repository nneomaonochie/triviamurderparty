open! Core
open! Async

type t =
  { mutable active_participants : (Socket.Address.Inet.t * Player.t) list
  ; mutable safe_players : (Socket.Address.Inet.t * Player.t) list
  ; mutable player_passwords_positions :
      (Socket.Address.Inet.t * Player.t * string * int) list
  }
[@@deriving sexp_of]

val create : unit -> t
val update_password : t -> client_ip:string -> query:string -> unit
