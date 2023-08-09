open! Core
open! Async

type t =
  { mutable chalice_poisoned : bool array
  ; mutable chalice_pickers : (Socket.Address.Inet.t * Player.t) list
  ; mutable chalice_choosers : (Socket.Address.Inet.t * Player.t) list
  }

val poison_chalice : int -> t -> unit
val create : unit -> t
val is_poisoned : int -> t -> bool
