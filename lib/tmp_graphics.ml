[@@@disable_unused_warnings]

open! Core
open! Async

module Color = struct
  let black = Graphics.rgb 000 000 000
  let green = Graphics.rgb 000 255 000
  let red = Graphics.rgb 255 000 000
  let blue = Graphics.rgb 000 000 255
  let dark_blue = Graphics.rgb 000 000 204
  let dark_red = Graphics.rgb 168 41 41

  let random =
    Graphics.rgb (Random.int 256) (Random.int 256) (Random.int 256)
  ;;
end

(* for both width and heights of the players *)
let player_block_size = 125

(* based off the number of players that are starting - index = numPlayers -
   1 *)
let player_starting_x_coords = [ 537; 400; 287; 150 ]

(* some of these numbers repeat... maybe i can optimize *)
let player_y_coord = 650
let display_beginning_instructions () = ()

(* this asks user input for players' names so that we can initiatilize the
   players and create a game *)
let player_creation_screen () =
  Graphics.open_graph (Printf.sprintf " %dx%d" 1200 800);
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

(* this places characters on a screen adjusting for the spacing depending on
   number of players *)
let display_players (players : Player.t list) =
  let rec paste_players x_coord (players_left : Player.t list) : unit =
    if List.length players_left = 0
    then ()
    else (
      let current_player = List.hd_exn players_left in
      let player_color = Color.random in
      Graphics.set_color player_color;
      Graphics.fill_rect
        x_coord
        player_y_coord
        player_block_size
        player_block_size;
      Graphics.moveto x_coord (player_y_coord + 50);
      Graphics.set_font
        "-*-fixed-medium-r-semicondensed--15-*-*-*-*-*-iso8859-1";
      Graphics.draw_string current_player.name;
      (*this removes the player we just instantiated *)
      let players_left = List.tl_exn players_left in
      paste_players (x_coord + 250) players_left)
  in
  if List.length players = 0
  then failwith "We need at least one player to display.";
  (* i want a list with the same items as player so i can pop things off
     without consequence *)
  let display_player_list = players in
  let num_players = List.length display_player_list in
  paste_players
    (List.nth_exn player_starting_x_coords (num_players - 1))
    display_player_list
;;

(* these are the graphics for specific game_kind*)
let create_trivia_graphics () = ()
let create_leaderboard_graphics (game : Game.t) = ()

let create_math_mayhem_graphics
  (participants : (Socket.Address.Inet.t * Player.t) list)
  =
  (* we only want to send the list of players to the clients, and we do not
     want to include client *)
  display_players
    (List.unzip participants |> fun (_, player_list) -> player_list);
  ()
;;

let create_decision_graphics () = ()
let create_clicker_graphics () = ()
