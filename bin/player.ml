open! Core

type t =
  { name : string
  ; mutable score : int
  ; mutable living : bool
      (* color : Color.t *)
      (* figure out later *)
  }
[@@deriving compare, equal, sexp_of]