open! Async

(** This module contains an implementation of a simple echo server. The
    client can send a string message to the server, which the server sends
    back to the client.

    [command] implements shell/console commands for starting up servers and
    clients. *)
val get_ip_address : Socket.Address.Inet.t -> string

val command : Async.Command.t
