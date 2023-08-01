[@@@disable_unused_warnings]

open! Core
open! Async

module Color = struct
  let black = Graphics.rgb 000 000 000
  let green = Graphics.rgb 000 255 000
  let red = Graphics.rgb 255 000 000
  let blue = Graphics.rgb 000 000 255
end

(* this asks user input for players' names so that we can initiatilize the
   players and create a game *)
let player_creation_screen () =
  Graphics.open_graph (Printf.sprintf " %dx%d" 1200 800);
  Graphics.set_color Color.black;
  Graphics.fill_rect 0 0 1200 800
;;

(* display instructions for how TMP works after the game is created *)
(* let display_beginning_instructions (game : Game.t) = (* display
   instruction for N seconds *) create_trivia_graphics game () ;; *)
(* these are the graphics for specific game_kind*)
let create_trivia_graphics () = ()
let create_math_mayhem_graphics () = ()
let create_decision_graphics () = ()
let create_clicker_graphics () = ()
