[@@@disable_unused_warnings]

open! Core
open! Async

module Color = struct
  let black = Graphics.rgb 000 000 000
  let green = Graphics.rgb 000 255 000
  let red = Graphics.rgb 255 000 000
  let blue = Graphics.rgb 000 000 255
end

(* for both width and heights of the players *)
let player_block_size = 125 
(* this asks user input for players' names so that we can initiatilize the
   players and create a game *)
let player_creation_screen () =
  Graphics.open_graph (Printf.sprintf " %dx%d" 1200 800);
  Graphics.set_color Color.black;
  Graphics.fill_rect 0 0 1200 800
;;

let display_beginning_instructions () = ()

(* this will deal with the spacing *)
let display_players players = 

  (* this is pseudocode - i could hardcode the starting points for each numPlayer and add the rect every X spaces,
     
  i could calculate based off the cebnter (odds is player in middle, evens is space in middle, do what makes sense)*)
  for (int x = top_left_x_coor; x < 1200 - top_left_x_coor; x += 250) { Graphics.fill rect x 700 player_block_size player_block_size;}





  (* spacing is based off of numPlayers *)
  (1200 / 2 ) - (125 / 2 ) -> this one player will be in the DEAD center (bc its based off the top left coordinate)

  if odd numplayers, center player is smack dab in middle, if even, the space is smack dab in middle
  (* BETWEEN players, the spacing is always 250 from the top left corner (in between its 150)*)
  
;;


(* these are the graphics for specific game_kind*)
let create_trivia_graphics () = ()

let create_math_mayhem_graphics
  (participants : (Socket.Address.Inet.t * Player.t) list)
  =
  display_players participants; 
  ()
;;

let create_decision_graphics () = ()
let create_clicker_graphics () = ()
