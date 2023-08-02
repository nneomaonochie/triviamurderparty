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
  Graphics.moveto 400 500;
  Graphics.set_color Color.red;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  Graphics.draw_string "Please enter your name into the console";
  Graphics.set_color Color.dark_blue;
  Graphics.draw_rect 375 400 600 200;
  Graphics.set_color Color.dark_red;
  Graphics.draw_arc
    (Random.int 500)
    (Random.int 500)
    (Random.int 500)
    (Random.int 500)
    (Random.int 500)
    (Random.int 500)
;;

(* recursively pastes other players from display_players*)
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
;;

(* this places characters on a screen adjusting for the spacing depending on
   number of players *)
(* returns a list of players and their corresponding x_coord*)
let display_players (players : (Socket.Address.Inet.t * Player.t) list)
  : ((Socket.Address.Inet.t * Player.t) * int) list
  =
  if List.length players = 0
  then failwith "We need at least one player to display.";
  (* I am making it exclusively a list of players *)
  let display_player_list =
    players
    (* List.unzip players |> fun (_, player_list) -> player_list *)
  in
  let num_players = List.length display_player_list in
  Graphics.set_color Color.black;
  Graphics.fill_rect 0 0 1200 800;
  paste_players
    (List.nth_exn player_starting_x_coords (num_players - 1))
    ~players_left:display_player_list
    ~player_positions:[]
;;

(* these are the graphics for specific game_kind*)
let create_trivia_graphics () = ()
let create_leaderboard_graphics (game : Game.t) = ()

(* pastes the arithmetic stuff where they are supposed to be *)
let paste_math_mayhem_qs
  (((client, player), x_coord) : (Socket.Address.Inet.t * Player.t) * int)
  (correct_answers : (string, int) Base.Hashtbl.t)
  =
  (* i need to fill in a rectangle to cover previous questions but i dont
     know the dimensiosn yet *)

  (* Graphics.set_color Color.black; Graphics.fill_rect *)
  Graphics.set_color Color.white;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  Graphics.moveto x_coord (player_y_coord - 50);
  let ques, ans = Math_mayhem.get_questions () in
  Graphics.draw_string ques;
  let client_ip = Game.get_ip_address client in
  (* if we already have client as a member, we update - if not we add *)
  print_s [%message (ques, ans : string * int)];
  Hashtbl.set
    correct_answers
    ~key:client_ip
    ~data:ans (* can you set values that werent already there *)
;;

(* draws the string but what is the answer? how about we create a map with
   client -> player -> score we need *)

let create_math_mayhem_graphics
  (participants : (Socket.Address.Inet.t * Player.t) list)
  =
  (* we only want to send the list of players to the clients, and we do not
     want to include client *)
  let player_positions = display_players participants in
  (* this is the hashtable that stores the correct answer to arithmetic
     questions as values *)
  let correct_answers : (string, int) Base.Hashtbl.t =
    Hashtbl.create
      ~growth_allowed:false
      ~size:(List.length player_positions)
      (module String)
  in
  (* this is the hashtable that tracks the amount of questions users have
     gotten correct so far *)
  let current_points : (string, int) Base.Hashtbl.t =
    Hashtbl.create
      ~growth_allowed:false
      ~size:(List.length player_positions)
      (module String)
  in
  (* the first questions are displayed *)
  paste_math_mayhem_qs (List.hd_exn player_positions) correct_answers;
  print_s [%message "" (correct_answers : (string, int) Base.Hashtbl.t)]
;;

let create_decision_graphics () = ()
let create_clicker_graphics () = ()
