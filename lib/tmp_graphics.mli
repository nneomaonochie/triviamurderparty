[@@@disable_unused_warnings]

open! Core
open! Async

val player_creation_screen : unit -> unit
val display_beginning_instructions : unit -> unit
val create_trivia_graphics : Game.t -> unit

val initialize_math_mayhem_graphics
  :  (Socket.Address.Inet.t * Player.t) list
  -> Game.t
  -> unit

val start_math_mayhem_intro : unit -> unit
val math_mayhem_player_response : Socket.Address.Inet.t -> string -> unit
val create_decision_graphics : unit -> unit
val start_pp_intro : (Socket.Address.Inet.t * Player.t) list -> unit

val initialize_password_pain_graphics
  :  participants:(Socket.Address.Inet.t * Player.t) list
  -> safe_players:(Socket.Address.Inet.t * Player.t) list
  -> unit

val show_correct_answer : Game.t -> unit
val pp_password_creation : Socket.Address.Inet.t -> string -> Game.t -> unit
