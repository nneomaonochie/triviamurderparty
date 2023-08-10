open! Core
open! Async

type t =
  { mutable chalice_poisoned : bool array
  ; mutable chalice_pickers : (Socket.Address.Inet.t * Player.t) list
  ; mutable chalice_choosers : (Socket.Address.Inet.t * Player.t) list
  }

let create () =
  { chalice_poisoned = Array.create ~len:4 false
  ; chalice_pickers = []
  ; chalice_choosers = []
  }
;;

(* this poisons the chalice when choosers choce a chalice to poison *)
let poison_chalice (i : int) (chalice : t) =
  if i - 1 < 4 && i - 1 >= 0
  then Array.set chalice.chalice_poisoned (i - 1) true
  else ()
;;

(* checks if a chalice picked is poisoned *)
let is_poisoned (i : int) (chalice : t) =
  Array.get chalice.chalice_poisoned (i - 1)
;;
