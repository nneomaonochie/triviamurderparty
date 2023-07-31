[@@@disable_unused_warnings]

open! Core
open! Async

(* [Protocol] defines the communication between the server and the client. *)
module Protocol : sig
  (* [Query] defines the type that the client sends to the server. Here, the
     query contains a string field, [query_message], which the server will
     echo back. *)

  (* this is for the queries that require a string (name creation,
     minigames) *)
  module Query_string : sig
    type t = { query_message : string } [@@deriving sexp_of]

    val to_string : t -> string
  end

  module Query_char : sig
    type t = { query_char : char } [@@deriving sexp_of]

    val to_char : t -> char
  end

  (* [Response] defines the type that the server sends back to the client.
     Here, the response contains a string field, [response_message]. *)
  module Response : sig
    type t = { response_message : string } [@@deriving sexp_of]
  end

  (* The [rpc] exposed here can be thought of as the interface between the
     server and the client. It dictates that clients can send [Query.t]s and
     receive [Response.t]s. *)
  val rpc_string : (Query_string.t, Response.t) Rpc.Rpc.t
  val rpc_char : (Query_char.t, Response.t) Rpc.Rpc.t
end = struct
  (* end of protocol *)

  module Query_string = struct
    (* Deriving "bin_io" here automatically gives us a way to convert values
       into a binary protocol, which lets us safely perform the input and
       output needed to send values between the client and the server. *)
    type t = { query_message : string } [@@deriving sexp_of, bin_io]

    let to_string { query_message } = query_message
  end

  module Query_char = struct
    (* Deriving "bin_io" here automatically gives us a way to convert values
       into a binary protocol, which lets us safely perform the input and
       output needed to send values between the client and the server. *)
    type t = { query_char : char } [@@deriving sexp_of, bin_io]

    let to_char { query_char } = query_char
  end

  module Response = struct
    type t = { response_message : string } [@@deriving sexp_of, bin_io]
  end

  let rpc_string =
    Rpc.Rpc.create
      ~name:"send-message"
      ~version:0
      ~bin_query:Query_string.bin_t
      ~bin_response:Response.bin_t
  ;;

  let rpc_char =
    Rpc.Rpc.create
      ~name:"send-message"
      ~version:0
      ~bin_query:Query_char.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Server = struct
  (* In the server, we have to define implementations for all of the RPCs the
     server will support. For each RPC, we need to provide a function that
     takes in the query type specified by the RPC and produces a response
     type. *)

  (* gets the query from the client *)
  let handle_query client query =
    Core.print_s
      [%message
        "Received query"
          (client : Socket.Address.Inet.t)
          (query : Protocol.Query.t)];
    (* changing this into a deferred type *)
    return
      { Protocol.Response.response_message =
          [%string
            "I have received your query! You said: %{query#Protocol.Query}"]
      }
  ;;

  let implementations =
    Rpc.Implementations.create_exn
      ~on_unknown_rpc:`Close_connection
      ~implementations:[ Rpc.Rpc.implement Protocol.rpc handle_query ]
  ;;

  (* add a separate RPC *)

  let serve port =
    let%bind server =
      Rpc.Connection.serve
        ~implementations
        ~initial_connection_state:(fun addr _conn -> addr)
        ~where_to_listen:(Tcp.Where_to_listen.of_port port)
        ()
    in
    Tcp.Server.close_finished server
  ;;
end

module Client = struct end

module RPC = struct
  (* we could use a RPC for asking a user's name (String), and getting a
     user's answer (char) *)
end
