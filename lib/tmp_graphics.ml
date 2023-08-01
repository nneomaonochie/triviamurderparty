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

(* display instructions for how TMP works after the game is created *)
<<<<<<< HEAD
(* let display_beginning_instructions (game : Game.t) = (* display
   instruction for N seconds *) create_trivia_graphics game () ;; *)
=======
let display_beginning_instructions () = ()

(* this will deal with the spacing *)
let display_players players = 
  (* spacing is based off of numPlayers *)
  
;;

>>>>>>> 6be2bb8d8160075b136a6403d6da49521773330b
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
