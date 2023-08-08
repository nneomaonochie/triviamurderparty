open! Core
open! Async

type t =
  { mutable chalice_poisoned : bool list
  ; mutable participants : (Socket.Address.Inet.t * Player.t) list
  }

let poison_chalice (i : int) (chalice : t) = ()
