[@@@disable_unused_warnings]

open! Core
open! Async

val player_creation_screen : unit -> unit
val display_beginning_instructions : Game.t -> unit
val create_trivia_graphics : unit -> unit

val create_math_mayhem_graphics
  :  (Socket.Address.Inet.t * Player.t) list
  -> unit

val create_decision_graphics : unit -> unit
val create_clicker_graphics : unit -> unit
