open! Core
open! Async

type t =
  { mutable chalice_poisoned : bool list
  ; mutable participants : (Socket.Address.Inet.t * Player.t) list
  ; mutable participants_choosing_chalices :
      (Socket.Address.Inet.t * Player.t) list
  }
