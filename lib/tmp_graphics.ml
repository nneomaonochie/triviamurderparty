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

(* these are the graphics for specific game_kind*)

(* this asks user input for players' names so that we can initiatilize the
   players and create a game *)
let player_creation_screen () =
  Graphics.open_graph " 1500x800";
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

let game =
  { Game.player_list = []
  ; game_type = Trivia (Triviaquestions.pick_random_question ())
  ; game_state = Ongoing
  }
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
    (* if player looks dead, put an x *)
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
  let display_player_list = players in
  let num_players = List.length display_player_list in
  Graphics.set_color Color.black;
  Graphics.fill_rect 0 0 1200 800;
  paste_players
    (List.nth_exn player_starting_x_coords (num_players - 1))
    ~players_left:display_player_list
    ~player_positions:[]
;;

let create_trivia_graphics (game : Game.t) =
  let d = display_players game.player_list in
  ();
  let question =
    match game.game_type with
    | Trivia q -> q
    | _ ->
      { Question.question = ""; answer_choices = []; correct_answer = "" }
  in
  Graphics.set_color Color.black;
  Graphics.fill_rect 0 0 1500 800;
  Graphics.set_color Color.white;
  Graphics.moveto 400 500;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  Graphics.draw_string question.question;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--24-*-*-*-*-*-iso8859-1";
  Graphics.moveto 300 400;
  Graphics.draw_string (List.nth_exn question.answer_choices 0);
  Graphics.moveto 350 350;
  Graphics.draw_string (List.nth_exn question.answer_choices 1);
  Graphics.moveto 900 350;
  Graphics.draw_string (List.nth_exn question.answer_choices 2);
  Graphics.moveto 950 300;
  Graphics.draw_string (List.nth_exn question.answer_choices 3)
;;

let create_leaderboard_graphics (game : Game.t) = ()

(* pastes the arithmetic stuff where they are supposed to be *)
let paste_math_mayhem_qs
  ~(correct_answers : (string, int) Base.Hashtbl.t)
  (((client, player), x_coord) : (Socket.Address.Inet.t * Player.t) * int)
  =
  Graphics.set_color Color.black;
  Graphics.fill_rect x_coord (player_y_coord - 50) 150 30;
  Graphics.set_color Color.white;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  Graphics.moveto x_coord (player_y_coord - 50);
  let ques, ans = Math_mayhem.get_questions () in
  Graphics.draw_string ques;
  let client_ip = Game.get_ip_address client in
  Hashtbl.set correct_answers ~key:client_ip ~data:ans
;;

let display_math_mayhem_points
  ~(current_points : (string, int) Base.Hashtbl.t)
  (((client, _), x_coord) : (Socket.Address.Inet.t * Player.t) * int)
  =
  (* replace the previous score *)
  Graphics.set_color Color.black;
  Graphics.fill_rect
    (x_coord + (player_block_size / 2))
    (player_y_coord - 120)
    50
    50;
  Graphics.set_color Color.dark_red;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--40-*-*-*-*-*-iso8859-1";
  Graphics.moveto (x_coord + (player_block_size / 2)) (player_y_coord - 120);
  Graphics.draw_string
    (Int.to_string
       (Hashtbl.find_exn current_points (Game.get_ip_address client)))
;;

(* when transitioning from Trivia to Math Mayhem, a function should be called
   to set up the intial graphics *)
let initialize_math_mayhem_graphics
  (participants : (Socket.Address.Inet.t * Player.t) list)
  =
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
  (* the scores are set to 0*)
  List.iter player_positions ~f:(fun ((c, _), _) ->
    Hashtbl.add_exn current_points ~key:(Game.get_ip_address c) ~data:0);
  (* the first questions and scores are displayed *)
  List.iter player_positions ~f:(display_math_mayhem_points ~current_points);
  List.iter player_positions ~f:(paste_math_mayhem_qs ~correct_answers)
;;

(* to match up what the user inputted and to change the screen, call this
   function - this is what players call in TMP_Server!!! *)
let math_mayhem_player_response client query =
  print_s
    [%message
      "Have a check to ensure ONLY players in the minigame are answering"];
  print_s
    [%message
      "We need a way to access the current_pints and current_nswers... \
       Maybe a global variable IS necessary for the time being"];
  print_s
    [%message
      "honestly that can work, all you really need is to re-assign the \
       global var in Graphics - better idea is to\n\
      \  use a record in Math_Mayhem!"]
;;

(* now we need to GET player response...*)

(* when we check for the correct answer, we are going to update the current
   points, display the new scorea and display the new questions*)

let create_decision_graphics () = ()
let create_clicker_graphics () = ()
