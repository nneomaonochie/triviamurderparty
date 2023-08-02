[@@@disable_unused_warnings]

open! Core
open! Async

(* for both width and heights of the players *)
let player_block_size = 125

(* based off the number of players that are starting - index = numPlayers -
   1 *)
let player_starting_x_coords = [ 537; 400; 287; 150 ]
let player_y_coord = 620
let display_beginning_instructions () = ()

(* this asks user input for players' names so that we can initiatilize the
   players and create a game *)
let player_creation_screen () =
  Graphics.open_graph " 1200x800";
  Graphics.set_color Color.black;
  Graphics.fill_rect 0 0 1200 800;
  Graphics.set_color Color.dark_red;
  let _result =
    List.init 1000 ~f:(fun _ ->
      Graphics.draw_arc
        (Random.int 1200)
        (Random.int 800)
        (Random.int 150)
        (Random.int 150)
        (Random.int 30)
        (Random.int 30))
  in
  ();
  Graphics.set_color Color.dark_blue;
  Graphics.draw_rect 375 400 600 200;
  Graphics.fill_rect 375 400 600 200;
  Graphics.moveto 400 500;
  Graphics.set_color Color.red;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  Graphics.draw_string "Please enter your name into the console"
;;

(* this places characters on a screen adjusting for the spacing depending on
   number of players *)
(* returns a list of players and their corresponding x_coord*)
let display_players (players : (Socket.Address.Inet.t * Player.t) list)
  : ((Socket.Address.Inet.t * Player.t) * int) list
  =
  let rec paste_players x_coord ~players_left ~player_positions
    : ((Socket.Address.Inet.t * Player.t) * int) list
    =
    if List.length players_left = 0
    then player_positions
    else (
      let (client, current_player) : Socket.Address.Inet.t * Player.t =
        List.hd_exn players_left
      in
      Graphics.set_color current_player.color;
      Graphics.fill_rect
        x_coord
        player_y_coord
        player_block_size
        player_block_size;
      (* if player looks dead, you need to visually show that *)
      Graphics.moveto x_coord 750;
      Graphics.set_font
        "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
      Graphics.draw_string current_player.name;
      (*this removes the player we just instantiated *)
      let players_left = List.tl_exn players_left in
      let player_positions =
        player_positions @ [ (client, current_player), x_coord ]
      in
      paste_players (x_coord + 250) ~players_left ~player_positions)
  in
  if List.length players = 0
  then failwith "We need at least one player to display.";
  (* I am making it exclusively a list of players *)
  let display_player_list =
    players
    (* List.unzip players |> fun (_, player_list) -> player_list *)
  in
  let num_players = List.length display_player_list in
  paste_players
    (List.nth_exn player_starting_x_coords (num_players - 1))
    ~players_left:display_player_list
    ~player_positions:[]
;;

(* these are the graphics for specific game_kind*)
let create_trivia_graphics (game : Game.t) =
  let d = display_players game.player_list in
  ();
  let question_to_display =
    match game.game_type with
    | Trivia q -> q
    | _ -> { question = ""; answer_choices = []; correct_answer = "" }
  in
  Graphics.moveto 500 500;
  Graphics.draw_string question_to_display.question
;;

let create_leaderboard_graphics (game : Game.t) = ()

(* pastes the arithmetic stuff where they are supposed to be *)
let paste_math_mayhem_qs
  ((player, x_coord) : Player.t * int)
  (correct_answers : (string, int) Base.Hashtbl.t)
  =
  Graphics.set_color Color.white;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  Graphics.moveto x_coord (player_block_size - 100);
  let ques, ans = "", "" (* Math_mayhem.get_questions ()*) in
  Graphics.draw_string ques
;;

(* draws the string but what is the answer? how about we create a map with
   client -> player -> score we need *)

let create_math_mayhem_graphics
  (participants : (Socket.Address.Inet.t * Player.t) list)
  =
  (* we only want to send the list of players to the clients, and we do not
     want to include client *)
  let player_positions = display_players participants in
  (* mapping IP adress (client) -> correct (int) answer *)
  let correct_answers : (string, int) Base.Hashtbl.t =
    Hashtbl.create
      ~growth_allowed:false
      ~size:(List.length player_positions)
      (module String)
  in
  ()
;;

(* the first questions are displayed *)
(* List.iter player_positions ~f:paste_math_mayhem_qs correct_answers *)

let create_decision_graphics () = ()
let create_clicker_graphics () = ()
