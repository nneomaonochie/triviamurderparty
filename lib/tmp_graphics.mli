open! Core
open! Async

val create_players : unit -> unit

val create_math_mayhem_graphics
  :  (Socket.Address.Inet.t * Player.t) list
  -> unit
