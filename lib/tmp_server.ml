[@@@disable_unused_warnings]

open! Core
open! Async

let game_stack = Stack.create ()

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

  (* this is for queries that only require one key *)
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
      ~name:"send-answer"
      ~version:0
      ~bin_query:Query_char.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Server : sig
  val command : Command.t
end = struct
  (* gets the query from the client *)
  let count = Array.create ~len:1 0

  let handle_query_string client query =
    let game : Game.t = Stack.pop_exn game_stack in
    let game =
      match game.game_state with
      | Player_Initializion ->
        let (g, finished_set_up) : Game.t * bool =
          Game.set_up_players
            client
            (Protocol.Query_string.to_string query)
            game
        in
        (* if we finished setting up players, display the graphics screen *)
        if Bool.equal finished_set_up true
        then Tmp_graphics.create_trivia_graphics g;
        g
      | Ongoing ->
        (match game.game_type with
         | Math_mayhem _ ->
           (* the players response to the question being shown *)
           Tmp_graphics.math_mayhem_player_response
             client
             (Protocol.Query_string.to_string query);
           game
         | _ -> game)
      | _ -> game
    in
    (* print_s [%message "" (count : int array)]; if count.(0) = 1 then
       Tmp_graphics.initialize_math_mayhem_graphics game.player_list;
       Array.set count 0 (count.(0) + 1); *)
    Stack.push game_stack game;
    Core.print_s
      [%message
        "Received query"
          (client : Socket.Address.Inet.t)
          (query : Protocol.Query_string.t)];
    (* changing this into a deferred type *)
    return
      { Protocol.Response.response_message =
          [%string
            "I have received your query! You said: \
             %{query#Protocol.Query_string}"]
      }
  ;;

  let find_player
    (player_list : (Socket.Address.Inet.t * Player.t) list)
    (client_ip : Socket.Address.Inet.t)
    =
    let desired_ip = Game.get_ip_address client_ip in
    let _, player =
      List.find_exn player_list ~f:(fun player ->
        let c, p = player in
        let comparison_ip = Game.get_ip_address c in
        if String.equal comparison_ip desired_ip then true else false)
    in
    player
  ;;

  let handle_query_char client query : Protocol.Response.t Deferred.t =
    let game = Stack.pop_exn game_stack in
    let question = game.game_type in
    let ip_addr = Game.get_ip_address client in
    let () =
      match question with
      | Trivia q ->
        let correct_ans = q.correct_answer in
        let players = game.player_list in
        let player_ans =
          String.of_char (Protocol.Query_char.to_char query)
        in
        let answer_is_correct = String.equal player_ans correct_ans in
        if answer_is_correct
        then (
          let player = find_player players client in
          player.score <- player.score + 1000)
        else ()
      | _ -> ()
    in
    print_s [%message "" (game : Game.t)];
    Stack.push game_stack game;
    Core.print_s
      [%message
        "Received query"
          (client : Socket.Address.Inet.t)
          (query : Protocol.Query_char.t)];
    (* changing this into a deferred type *)
    return
      { Protocol.Response.response_message =
          [%string "I have received your query! You said: CHAR"]
      }
  ;;

  let implementations (game : Game.t)
    : Socket.Address.Inet.t Rpc.Implementations.t
    =
    Rpc.Implementations.create_exn
      ~on_unknown_rpc:`Close_connection
      ~implementations:
        [ Rpc.Rpc.implement Protocol.rpc_string handle_query_string
        ; Rpc.Rpc.implement Protocol.rpc_char handle_query_char
        ]
  ;;

  (* idea: add an admin command for manual intervention in case the timing
     flops *)

  let serve port (game : Game.t) =
    let%bind server =
      Rpc.Connection.serve
        ~implementations:(implementations game)
        ~initial_connection_state:(fun addr _conn -> addr)
        ~where_to_listen:(Tcp.Where_to_listen.of_port port)
        ()
    in
    Tcp.Server.close_finished server
  ;;

  let main =
    [%map_open.Command
      let () = return ()
      and port =
        flag
          "-port"
          (required int)
          ~doc:"INT port that the server should listen on"
      in
      fun () ->
        (* this is where we do our beginning functions *)
        let game : Game.t = Game.create () in
        Stack.push game_stack game;
        let bool = Tmp_graphics.player_creation_screen () in
        ();
        (* to do later: intialize_graphics *)
        serve port game]
  ;;

  (* the first call with string will be to change the PC names *)

  let command =
    Command.async
      ~summary:"start rpc server"
      main
      ~behave_nicely_in_pipeline:true
  ;;
end

module Client : sig
  val command : Command.t
end = struct
  (* In the client, we need to define a way to fire (or dispatch) the RPCs
     that we care about. This requires knowing how to communicate to the
     server (by knowing the server address), constructing the query type, and
     doing something with the response that the server gives back. *)
  let send_message server_addr ~message =
    Rpc.Connection.with_client
      (Tcp.Where_to_connect.of_host_and_port server_addr)
      (fun connection ->
      let%map.Deferred.Or_error response =
        Rpc.Rpc.dispatch
          Protocol.rpc_string
          connection
          { query_message = message }
      in
      Core.print_s
        [%message "Received response" (response : Protocol.Response.t)])
    >>| Result.ok_exn
  ;;

  let send_char server_addr ~charc =
    Rpc.Connection.with_client
      (Tcp.Where_to_connect.of_host_and_port server_addr)
      (fun connection ->
      let%map.Deferred.Or_error response =
        Rpc.Rpc.dispatch Protocol.rpc_char connection { query_char = charc }
      in
      Core.print_s
        [%message "Received response" (response : Protocol.Response.t)])
    >>| Result.ok_exn
  ;;

  let send_message_command =
    Command.async_or_error
      ~summary:"send single message to server"
      [%map_open.Command
        let () = return ()
        and server_addr =
          flag
            "-server"
            (required host_and_port)
            ~doc:"HOST_AND_PORT server to query (e.g. localhost:1337)"
        and message =
          flag
            "-message"
            (required string)
            ~doc:"STRING message to send to server"
        in
        fun () -> send_message server_addr ~message]
      ~behave_nicely_in_pipeline:true
  ;;

  let send_char_command =
    Command.async_or_error
      ~summary:"send single char to server"
      [%map_open.Command
        let () = return ()
        and server_addr =
          flag
            "-server"
            (required host_and_port)
            ~doc:"HOST_AND_PORT server to query (e.g. localhost:1337)"
        and charc =
          flag "-char" (required char) ~doc:"CHAR message to send to server"
        in
        fun () -> send_char server_addr ~charc]
      ~behave_nicely_in_pipeline:true
  ;;

  let command =
    Command.group
      ~summary:"rpc client"
      [ "send-message", send_message_command
      ; "send-char", send_char_command
      ]
  ;;
end

let command =
  Command.group
    ~summary:"simple rpc client and server application"
    [ "server", Server.command; "client", Client.command ]
;;
