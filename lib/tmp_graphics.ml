[@@@disable_unused_warnings]

open! Core
open! Async

(* for both width and heights of the players *)
let player_block_size = 125

(* mutable instances of minigames that must be reset each time a minigame is
   run *)
let current_math_mayhem_hashtables = Math_mayhem.create ()
let current_pp_state = Password_pain.create ()
let current_chalice_state = Chalices.create ()
let final_round_category = Final_round.pick_random_question ()

(* based off the number of players that are starting - index = numPlayers -
   1 *)
let player_starting_x_coords = [ 687; 550; 437; 300 ]

(* the standard y coordinate for where players are *)
let player_y_coord = 620

(* displays the very beginning where they ask players to submit their
   names *)
let player_creation_screen () =
  Graphics.open_graph " 1500x800";
  Graphics.set_color Color.black;
  Graphics.fill_rect 0 0 1500 800;
  Graphics.set_color Color.dark_red;
  let _result =
    List.init 1000 ~f:(fun _ ->
      Graphics.draw_arc
        (Random.int 1500)
        (Random.int 800)
        (Random.int 150)
        (Random.int 150)
        (Random.int 30)
        (Random.int 30))
  in
  ();
  Graphics.set_color Color.dark_red;
  Graphics.draw_rect 375 400 600 200;
  Graphics.fill_rect 375 400 600 200;
  Graphics.moveto 400 500;
  Graphics.set_color Color.red;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  Graphics.draw_string "Please enter your name into the console"
;;

(* if players loser a game, it displays the losing players at the front *)
let display_losers loser_list =
  let rec dpl x_coord loser_list =
    if List.length loser_list = 0
    then ()
    else (
      let (_, (curr_player : Player.t)), _ = List.hd_exn loser_list in
      Graphics.set_color curr_player.color;
      (* at this point the player should be dead *)
      Graphics.fill_rect x_coord 450 player_block_size player_block_size;
      Graphics.moveto x_coord 600;
      Graphics.set_font
        "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
      Graphics.draw_string curr_player.name;
      let loser_list = List.tl_exn loser_list in
      dpl (x_coord + 250) loser_list)
  in
  Graphics.set_color Color.black;
  Graphics.fill_rect 0 0 1500 800;
  if List.length loser_list > 0
  then (
    dpl
      (List.nth_exn player_starting_x_coords (List.length loser_list - 1))
      loser_list;
    Graphics.set_color Color.dark_red;
    Graphics.moveto 650 500;
    Graphics.set_font
      "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
    Graphics.draw_string "You died.")
;;

(* this is the skull that should be shown on top of the player*)
let draw_skull (x_coord : int) (y_coord : int) =
  Graphics.set_color Color.skeleton_gray;
  Graphics.fill_circle (x_coord + 60) (y_coord + 75) 50;
  Graphics.fill_rect (x_coord + 35) y_coord 50 50;
  Graphics.set_color Color.black;
  Graphics.fill_rect (x_coord + 48) y_coord 5 25;
  Graphics.fill_rect (x_coord + 68) y_coord 5 25;
  Graphics.fill_circle (x_coord + 80) (y_coord + 75) 13;
  Graphics.fill_circle (x_coord + 40) (y_coord + 75) 13
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
    (* if player looks dead, then we draw a skull *)
    if Bool.equal current_player.living false
    then draw_skull x_coord player_y_coord;
    Graphics.set_color current_player.color;
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
  Graphics.fill_rect 0 0 1500 800;
  paste_players
    (List.nth_exn player_starting_x_coords (num_players - 1))
    ~players_left:display_player_list
    ~player_positions:[]
;;

(* this is the coordinates of the questions for the trivia portion *)
let get_correct_q_coords (game : Game.t) =
  let correct_ans =
    match game.game_type with Trivia q -> q.correct_answer | _ -> ""
  in
  match correct_ans with
  | "Q" -> 300, 400
  | "W" -> 350, 350
  | "E" -> 900, 350
  | "R" -> 950, 300
  | _ -> 0, 0
;;

(* this reveals the correct answer of the trivia portion *)
let show_correct_answer (game : Game.t) =
  Graphics.set_color Color.black;
  Graphics.fill_rect 0 0 1500 800;
  let question =
    match game.game_type with
    | Trivia q -> q
    | _ ->
      { Question.question = ""; answer_choices = []; correct_answer = "" }
  in
  let correct_ans =
    List.find_exn question.answer_choices ~f:(fun q ->
      let first_char = String.get q 0 in
      let first_char = String.of_char first_char in
      if String.equal first_char question.correct_answer then true else false)
  in
  Graphics.moveto 650 400;
  Graphics.set_color Color.white;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  Graphics.draw_string correct_ans;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--40-*-*-*-*-*-iso8859-1";
  Graphics.moveto 700 700;
  Graphics.set_color Color.green;
  Graphics.draw_string "Correct Answer"
;;

(* this is the winner *)
let display_winner (c, (player : Player.t)) =
  Graphics.set_color Color.black;
  Graphics.fill_rect 0 0 1500 800;
  (* let winner = List.hd_exn game.player_list in let _, p = winner in *)
  let d = display_players [ c, player ] in
  Graphics.set_color Color.green;
  Graphics.moveto 680 400;
  Graphics.draw_string "The Winner"
;;

(* displays the bonus for living players being alive *)
let display_alive_bonus (game : Game.t) =
  Graphics.set_color Color.black;
  Graphics.fill_rect 0 0 1500 800;
  let coords = display_players game.player_list in
  let alive_players =
    List.filter coords ~f:(fun ((c, p), x_coord) ->
      if p.living then true else false)
  in
  List.iter alive_players ~f:(fun ((c, p), x_coord) ->
    p.score <- p.score + 3000);
  List.iter alive_players ~f:(fun ((c, p), x_coord) ->
    Graphics.moveto (x_coord + 20) 550;
    Graphics.set_color Color.green;
    Graphics.draw_string "+3000")
;;

(* This is the core logic that actually runs the game. We have implemented
   all of this for you, but feel free to read this file as a reference. *)
let every seconds ~f ~stop =
  let open Async in
  (* recursive loop *)
  let rec loop () =
    if !stop
    then return ()
    else
      Clock.after (Time_float.Span.of_sec seconds)
      >>= fun () ->
      f ();
      loop ()
  in
  don't_wait_for (loop ())
;;

let animation_shift (player : Player.t) ~current_x ~y =
  Graphics.set_color player.color;
  Graphics.fill_rect current_x y player_block_size player_block_size;
  Graphics.set_color Color.black;
  Graphics.fill_rect current_x y 20 player_block_size
;;

(* returns places of where places should be *)
let shift_players
  (players : (Socket.Address.Inet.t * Player.t * int * int * int) list)
  =
  let new_p =
    List.map players ~f:(fun (c, pl, x, y, num_spaces) ->
      Graphics.set_color Color.black;
      Graphics.fill_rect x y 125 (125 + 200);
      (* the + 200 is to get rid of the name *)
      Graphics.set_color pl.color;
      (* adjust the y so corners are not touching later *)
      let new_x = x + (player_block_size * num_spaces) in
      (* animation_shift pl ~current_x:x ~x_stop:new_x ~y; *)
      let stop_animation = ref false in
      let coord = Array.create ~len:1 x in
      every
        0.01
        ~f:(fun () ->
          animation_shift pl ~current_x:coord.(0) ~y;
          Array.set coord 0 (coord.(0) + 10);
          if coord.(0) >= new_x then stop_animation := true)
        ~stop:stop_animation;
      (* Graphics.fill_rect new_x y player_block_size player_block_size; *)
      if not pl.living then draw_skull x y;
      Graphics.set_color pl.color;
      Graphics.moveto (new_x + 20) (y + 150);
      Graphics.set_font
        "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
      Graphics.draw_string pl.name;
      if not pl.living then draw_skull new_x y;
      c, pl, new_x, y, num_spaces)
  in
  final_round_category.final_players <- new_p
;;

let display_final_round_question () =
  let current_category = Final_round.pick_random_question () in
  (* the category fill rect*)
  Graphics.set_color Color.yellow;
  Graphics.fill_rect 1100 225 350 100;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  Graphics.set_color Color.black;
  Graphics.moveto 1100 225;
  Graphics.draw_string current_category.category;
  (* create a list of all the answer choices *)
  let all_answer_choices =
    current_category.right_answers @ current_category.wrong_answers
  in
  let rec display_answer_choices
    (answers : string list)
    ~(ind : int)
    ~(y : int)
    =
    if ind = 3
    then ()
    else (
      let current_choice = List.random_element_exn answers in
      let choice_letter =
        match ind with 0 -> "Q: " | 1 -> "W: " | 2 -> "E: " | _ -> "ERROR"
      in
      if List.mem
           current_category.right_answers
           current_choice
           ~equal:String.equal
      then
        current_category.char_placements
          <- current_category.char_placements @ [ true ]
      else
        current_category.char_placements
          <- current_category.char_placements @ [ false ];
      Graphics.set_color Color.yellow;
      Graphics.fill_rect 1100 y 350 50;
      Graphics.set_font
        "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
      Graphics.set_color Color.black;
      Graphics.moveto 1110 y;
      Graphics.draw_string (choice_letter ^ current_choice);
      (* we want to remove the choice we just displayed in the answer choices
         so we dont repeat answer choices *)
      let answers =
        List.filter answers ~f:(fun choice ->
          not (String.equal choice current_choice))
      in
      display_answer_choices answers ~ind:(ind + 1) ~y:(y - 60))
  in
  display_answer_choices all_answer_choices ~ind:0 ~y:150;
  final_round_category.category <- current_category.category;
  final_round_category.right_answers <- current_category.right_answers;
  final_round_category.wrong_answers <- current_category.wrong_answers;
  final_round_category.char_placements <- current_category.char_placements
;;

(* this handles the user's input for answering final round stuff *)
(* we'll use timers? *)
let final_round_user_input client query (game : Game.t) =
  let client_ip = Game.get_ip_address client in
  (* we identify that the player has not previously guessed yet *)
  if List.exists
       final_round_category.player_guesses
       ~f:(fun (c, _, _, has_guessed) ->
       String.equal client_ip (Game.get_ip_address c) && not has_guessed)
  then (
    let query = String.uppercase query in
    let player_answers_chars =
      if String.equal query "NONE" then [] else String.to_list query
    in
    let player_answers_bool_ar = Array.create ~len:3 false in
    (*[ false; false; false ] in*)
    List.iter player_answers_chars ~f:(fun char ->
      let ind = match char with 'Q' -> 0 | 'W' -> 1 | 'E' -> 2 | _ -> 10 in
      if ind <= 2 then Array.set player_answers_bool_ar ind true);
    let player_answer_bools = Array.to_list player_answers_bool_ar in
    (* we put the players answer in the array of player guesses *)
    final_round_category.player_guesses
      <- List.map
           final_round_category.player_guesses
           ~f:(fun (c, pl, bool_list, has_guessed) ->
           if String.equal client_ip (Game.get_ip_address c)
           then c, pl, player_answer_bools, has_guessed
           else c, pl, bool_list, has_guessed))
;;

(* blacks out the incorect answers that are on the board *)
let rec reveal_fr_answer correct_char_bools ~ind ~y_coord =
  Graphics.set_color Color.black;
  if ind = 3
  then ()
  else (
    if not (List.nth_exn correct_char_bools ind)
    then Graphics.fill_rect 1100 y_coord 350 50;
    reveal_fr_answer correct_char_bools ~ind:(ind + 1) ~y_coord:(y_coord - 60))
;;

(* evaluates how correct each player guess was to the real answer *)
let calc_fr_answers () =
  List.iter
    final_round_category.player_guesses
    ~f:(fun (c, _, player_answer_bools, _) ->
    let client_ip = Game.get_ip_address c in
    (* determines if users made the right selections by comparing it with the
       char placements list in the final round t *)
    let num_correct =
      List.fold2_exn
        player_answer_bools
        final_round_category.char_placements
        ~init:0
        ~f:(fun num_correct user_response actual_response ->
        if Bool.equal user_response actual_response
        then num_correct + 1
        else num_correct)
    in
    (* adjusting this to number of spaces *)
    final_round_category.final_players
      <- List.map
           final_round_category.final_players
           ~f:(fun (cl, pl, x, y, num_spaces) ->
           if String.equal client_ip (Game.get_ip_address cl)
           then cl, pl, x, y, num_correct
           else cl, pl, x, y, num_spaces));
  reveal_fr_answer final_round_category.char_placements ~ind:0 ~y_coord:150;
  let span = Time_ns.Span.of_sec 2.0 in
  let%bind () = Clock_ns.after span in
  shift_players final_round_category.final_players;
  return ()
;;

(* this starts the sequence of the final round *)
let final_round_round () =
  display_final_round_question ();
  let span = Time_ns.Span.of_sec 15.0 in
  let%bind () = Clock_ns.after span in
  calc_fr_answers ()
;;

(* these are the graphics for the final round *)
let display_final_round (game : Game.t) : unit Deferred.t =
  Graphics.set_color Color.black;
  Graphics.fill_rect 0 0 1500 800;
  (* set the inital spaces -> how much they increase by will be set in
     shift_players*)
  (* the ind is the y-coordinate *)
  let fr_players =
    List.mapi (Game.get_players_by_score game) ~f:(fun ind (c, pl) ->
      ( c
      , pl
      , player_block_size * ((*List.length game.player_list*) 4 - ind)
      , player_y_coord - (player_block_size * ind) - 50
      , 0 ))
  in
  shift_players fr_players;
  final_round_category.final_players <- fr_players;
  (* now we display the first categories *)
  (* LOOP THIS*)
  let%bind () =
    Deferred.repeat_until_finished () (fun () ->
      let continue_final_round =
        not
          (List.exists
             final_round_category.final_players
             ~f:(fun (_, _, x, _, _) -> x >= 1500))
      in
      if continue_final_round
      then (
        (* let span = Time_ns.Span.of_sec 15.0 in let%bind () =
           Clock_ns.after span in *)
        final_round_category.player_guesses
          <- List.map
               final_round_category.final_players
               ~f:(fun (c, pl, _, _, _) ->
               c, pl, [ false; false; false ], false);
        let%bind () = final_round_round () in
        return (`Repeat ()))
      else return (`Finished ()))
  in
  let client, player, _, _, _ =
    List.find_exn
      final_round_category.final_players
      ~f:(fun (c, pl, x, y, num_spaces) -> x >= 1500)
  in
  (* if finished *)
  game.game_state <- Game_over;
  display_winner (client, player);
  return ()
;;

let fr_instructions_2 (game : Game.t) =
  Graphics.set_color Color.pale_blue;
  Graphics.fill_rect 500 250 600 300;
  Graphics.set_color Color.black;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.moveto 600 425;
  Graphics.draw_string "If you think none";
  Graphics.moveto 535 375;
  Graphics.draw_string "are correct, then type";
  Graphics.moveto 700 310;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--60-*-*-*-*-*-iso8859-1";
  Graphics.draw_string "\"NONE\"";
  let span = Time_ns.Span.of_sec 4.0 in
  let%bind () = Clock_ns.after span in
  display_final_round game
;;

let final_round_instructions_1 (game : Game.t) =
  Graphics.set_color Color.pale_blue;
  Graphics.fill_rect 500 250 600 300;
  Graphics.set_color Color.black;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.moveto 635 450;
  Graphics.draw_string "Put the letters";
  Graphics.moveto 535 400;
  Graphics.draw_string "you think are correct";
  Graphics.moveto 635 350;
  Graphics.draw_string "as one string.";
  Graphics.moveto 515 300;
  Graphics.draw_string "Ex: \"QE\" or \"W\" or \"QWE\"";
  let span = Time_ns.Span.of_sec 4.0 in
  let%bind () = Clock_ns.after span in
  fr_instructions_2 game
;;

let final_round_title () =
  Graphics.set_color Color.black;
  Graphics.fill_rect 0 0 1500 800;
  Graphics.set_color Color.pale_blue;
  Graphics.fill_rect 500 250 500 300;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--60-*-*-*-*-*-iso8859-1";
  Graphics.set_color Color.black;
  Graphics.moveto 575 375;
  Graphics.draw_string "Final Round"
;;

let final_round_intro (game : Game.t) =
  final_round_title ();
  let span = Time_ns.Span.of_sec 3.0 in
  let%bind () = Clock_ns.after span in
  final_round_instructions_1 game
;;

(* the graphics for creating the trivia questions *)
let create_trivia_graphics (game : Game.t) =
  if List.fold game.player_list ~init:0 ~f:(fun living_counter (_, p) ->
       if p.living then living_counter + 1 else living_counter)
     < 2
     || game.questions_asked > 9
  then (
    game.game_state <- Final_round;
    display_alive_bonus game;
    let span = Time_ns.Span.of_sec 3.0 in
    let%bind () = Clock_ns.after span in
    final_round_intro game)
  else (
    let players = game.player_list in
    List.iter players ~f:(fun (_, player) ->
      player.answered_mr_question_wrong <- false;
      player.answered_mr_question <- false);
    Graphics.set_color Color.black;
    Graphics.fill_rect 0 0 1500 800;
    let d = display_players game.player_list in
    let question =
      match game.game_type with
      | Trivia q -> q
      | _ ->
        { Question.question = ""; answer_choices = []; correct_answer = "" }
    in
    Graphics.set_color Color.white;
    Graphics.moveto 400 500;
    Graphics.set_font
      "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
    Graphics.draw_string question.question;
    Graphics.set_font
      "-*-fixed-medium-r-semicondensed--24-*-*-*-*-*-iso8859-1";
    Graphics.moveto 300 400;
    Graphics.draw_string (List.nth_exn question.answer_choices 0);
    Graphics.moveto 350 350;
    Graphics.draw_string (List.nth_exn question.answer_choices 1);
    Graphics.moveto 900 350;
    Graphics.draw_string (List.nth_exn question.answer_choices 2);
    Graphics.moveto 950 300;
    Graphics.draw_string (List.nth_exn question.answer_choices 3);
    return ())
;;

(* this is the graphics that displays the players in order of descending
   scores *)
let create_leaderboard_graphics (game : Game.t) =
  let rec display_pl_leaderboard x_coord y_coord players =
    if List.length players = 0
    then ()
    else (
      let curr_player : Player.t = List.hd_exn players in
      Graphics.set_color curr_player.color;
      Graphics.set_color curr_player.color;
      Graphics.fill_rect x_coord y_coord player_block_size player_block_size;
      Graphics.moveto x_coord (y_coord + 150);
      Graphics.set_font
        "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
      Graphics.draw_string curr_player.name;
      Graphics.set_color Color.white;
      Graphics.moveto (x_coord + 35) (y_coord - 65);
      Graphics.draw_string (Int.to_string curr_player.score);
      if Bool.equal curr_player.living false
      then draw_skull x_coord (y_coord - 20);
      let players = List.tl_exn players in
      display_pl_leaderboard (x_coord + 250) (y_coord - 100) players)
  in
  Graphics.set_color Color.black;
  Graphics.fill_rect 0 0 1500 800;
  (* this sorts the players by their score, so the player with the highest
     score is at index 0*)
  let players_by_score =
    game.player_list
    |> List.map ~f:snd
    |> List.map ~f:(fun player -> player.score, player)
    |> List.sort ~compare:[%compare: int * Player.t]
    |> List.map ~f:snd
    |> List.rev
  in
  display_pl_leaderboard 300 600 players_by_score;
  (* transition to *)
  game.game_type
    <- Trivia
         { Question.question = ""; answer_choices = []; correct_answer = "" };
  Game.ask_question game;
  let span = Time_ns.Span.of_sec 5.0 in
  let%bind () = Clock_ns.after span in
  create_trivia_graphics game
;;

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

(* cases: [maybe make it a list of losers] 1. multiple players have the
   lowest score 2. ALL players hve the lowest score *)
let math_mayhem_calc_scores (game : Game.t) =
  let get_int (i, s) = i in
  let get_ip (_, cs) = cs in
  (* if only one player is playing, then they need to get at least 15 to
     survive *)
  if Hashtbl.length current_math_mayhem_hashtables.current_points = 1
  then (
    Hashtbl.iteri
      current_math_mayhem_hashtables.current_points
      ~f:(fun ~key ~data ->
      if data < 15
      then (
        (* adjusting the player in the player list *)
        List.iter game.player_list ~f:(fun (c, p) ->
          if String.equal (Game.get_ip_address c) key then p.living <- false);
        display_losers
          [ List.find_exn
              current_math_mayhem_hashtables.player_positions
              ~f:(fun ((c, pl), _) ->
              pl.living <- false;
              String.equal (Game.get_ip_address c) key)
          ]));
    let span = Time_ns.Span.of_sec 5.0 in
    let%bind () = Clock_ns.after span in
    create_leaderboard_graphics game)
  else (
    let worst_perf = Array.create ~len:1 (Int.max_value, "") in
    (* tries to find player with lowest scores*)
    Hashtbl.iteri
      current_math_mayhem_hashtables.current_points
      ~f:(fun ~key ~data ->
      if data < get_int worst_perf.(0) then Array.set worst_perf 0 (data, key));
    let (c, losing_player), x_coord =
      List.find_exn
        current_math_mayhem_hashtables.player_positions
        ~f:(fun ((c, _), _) ->
        String.equal (Game.get_ip_address c) (get_ip worst_perf.(0)))
    in
    (* the loser player dies*)
    Player.player_loses losing_player;
    display_losers [ (c, losing_player), x_coord ];
    let span = Time_ns.Span.of_sec 5.0 in
    let%bind () = Clock_ns.after span in
    create_leaderboard_graphics game)
;;

(* displays the title of Math Mayhem *)
let display_math_mayhem_title () =
  Graphics.set_color Color.black;
  Graphics.fill_rect 0 0 1500 800;
  Graphics.set_color Color.white;
  Graphics.fill_rect 500 250 500 300;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--60-*-*-*-*-*-iso8859-1";
  Graphics.set_color Color.black;
  Graphics.moveto 600 375;
  Graphics.draw_string "Math Mayhem"
;;

let display_math_mayhem_instructions () =
  Graphics.set_color Color.white;
  Graphics.fill_rect 500 250 500 300;
  Graphics.set_color Color.black;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.moveto 635 400;
  Graphics.draw_string "The worst";
  Graphics.moveto 535 350;
  Graphics.draw_string "mathematician dies"
;;

let start_math_mayhem_intro () =
  display_math_mayhem_title ();
  let span = Time_ns.Span.of_sec 3.0 in
  Clock_ns.run_after span (fun () -> display_math_mayhem_instructions ()) ()
;;

(* when transitioning from Trivia to Math Mayhem, a function should be called
   to set up the intial graphics *)
let initialize_math_mayhem_graphics
  (participants : (Socket.Address.Inet.t * Player.t) list)
  (game : Game.t)
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
  (* the scores are initially set to 0 *)
  List.iter player_positions ~f:(fun ((c, _), _) ->
    Hashtbl.add_exn current_points ~key:(Game.get_ip_address c) ~data:0);
  (* the first questions and scores are displayed *)
  List.iter player_positions ~f:(display_math_mayhem_points ~current_points);
  List.iter player_positions ~f:(paste_math_mayhem_qs ~correct_answers);
  current_math_mayhem_hashtables.correct_answers <- correct_answers;
  current_math_mayhem_hashtables.current_points <- current_points;
  current_math_mayhem_hashtables.player_positions <- player_positions;
  (* players have 30 seconds to accumulate as many points as possible *)
  let span = Time_ns.Span.of_sec 40.0 in
  let%bind () = Clock_ns.after span in
  math_mayhem_calc_scores game
;;

(* when clients send queries in this mode, we update the screen based off of
   their reponses *)
let math_mayhem_player_response client query =
  (* this is assuming that i correctly altered the global math_mayhem record
     var in graphics *)
  let client_ip = Game.get_ip_address client in
  (* check if client is among the participants in this minigame *)
  if List.exists
       current_math_mayhem_hashtables.player_positions
       ~f:(fun ((c, _), _) -> String.equal (Game.get_ip_address c) client_ip)
  then
    if (* we need to check if a user got the question correct *)
       String.equal
         query
         (Int.to_string
            (Hashtbl.find_exn
               current_math_mayhem_hashtables.correct_answers
               client_ip))
    then (
      (* we increase their points and give them a new question and point
         total *)
      let prev_score =
        Hashtbl.find_exn
          current_math_mayhem_hashtables.current_points
          client_ip
      in
      Hashtbl.set
        current_math_mayhem_hashtables.current_points
        ~key:client_ip
        ~data:(prev_score + 1);
      let curr_client =
        List.find_exn
          current_math_mayhem_hashtables.player_positions
          ~f:(fun ((c, _), _) ->
          String.equal (Game.get_ip_address c) client_ip)
      in
      let (_, curr_player), _ = curr_client in
      (* each correct answer increases the score by 25 *)
      curr_player.score <- curr_player.score + 25;
      display_math_mayhem_points
        ~current_points:current_math_mayhem_hashtables.current_points
        curr_client;
      paste_math_mayhem_qs
        ~correct_answers:current_math_mayhem_hashtables.correct_answers
        curr_client)
;;

(* this draws the chalices onto the screen *)
let draw_chalices () =
  Graphics.set_color Color.black;
  Graphics.fill_rect 0 0 1500 800;
  Graphics.set_color (Color.random ());
  Graphics.fill_rect 175 300 50 200;
  Graphics.fill_arc 200 500 100 50 (-180) 0;
  Graphics.moveto 190 225;
  Graphics.draw_string "1";
  Graphics.set_color (Color.random ());
  Graphics.fill_rect 550 300 50 200;
  Graphics.fill_arc 575 500 100 50 (-180) 0;
  Graphics.moveto 565 225;
  Graphics.draw_string "2";
  Graphics.set_color (Color.random ());
  Graphics.fill_rect 925 300 50 200;
  Graphics.fill_arc 950 500 100 50 (-180) 0;
  Graphics.moveto 940 225;
  Graphics.draw_string "3";
  Graphics.set_color (Color.random ());
  Graphics.fill_rect 1300 300 50 200;
  Graphics.fill_arc 1325 500 100 50 (-180) 0;
  Graphics.moveto 1315 225;
  Graphics.draw_string "4";
  return ()
;;

(* displays the chalice title screen*)
let display_chalice_title () =
  Graphics.set_color Color.pastel_purple;
  Graphics.fill_rect 500 250 500 300;
  Graphics.set_color Color.white;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.moveto 635 400;
  Graphics.draw_string "Chalices";
  let span = Time_ns.Span.of_sec 3.0 in
  let%bind () = Clock_ns.after span in
  draw_chalices ()
;;

(* this displays the instructions for the safe players who choose a chalice
   to poison *)
let display_chalice_title_for_safe_players () =
  Graphics.set_color Color.black;
  Graphics.fill_rect 0 0 1500 800;
  Graphics.set_color Color.pastel_purple;
  Graphics.fill_rect 500 250 500 300;
  Graphics.set_color Color.white;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.moveto 535 400;
  Graphics.draw_string "Safe players, pick a";
  Graphics.moveto 535 350;
  Graphics.draw_string "chalice to poison"
;;

(* this displays the instructions for players who must pick a chalice to
   drink *)
let display_chalice_title_for_endangered_players () =
  Graphics.set_color Color.pastel_purple;
  Graphics.fill_rect 500 250 500 300;
  Graphics.set_color Color.white;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.moveto 535 450;
  Graphics.draw_string "Endangered players,";
  Graphics.moveto 535 400;
  Graphics.draw_string "pick a chalice";
  Graphics.moveto 550 350;
  Graphics.draw_string "Choose wrong,";
  Graphics.moveto 550 300;
  Graphics.draw_string "then you die"
;;

(* this ends the chalice minigame *)
let chalice_ending
  (losers : ((Socket.Address.Inet.t * Player.t) * int) list)
  (game : Game.t)
  =
  let losers = List.filter losers ~f:(fun ((_, p), _) -> not p.living) in
  display_losers losers;
  let span = Time_ns.Span.of_sec 3.0 in
  let%bind () = Clock_ns.after span in
  create_leaderboard_graphics game
;;

(* handles the queries from the players who have to pick a cup to drink *)
let chalice_picking client query (game : Game.t) =
  let coords = display_players current_chalice_state.chalice_pickers in
  display_chalice_title_for_endangered_players ();
  let client_ip = Game.get_ip_address client in
  if List.exists current_chalice_state.chalice_pickers ~f:(fun (c, _) ->
       String.equal client_ip (Game.get_ip_address c))
  then (
    let client, player =
      List.find_exn current_chalice_state.chalice_pickers ~f:(fun (c, _) ->
        String.equal client_ip (Game.get_ip_address c))
    in
    let chalice_number = Int.of_string query in
    if chalice_number > 0 && chalice_number < 5
    then (
      if Chalices.is_poisoned chalice_number current_chalice_state
      then player.living <- false
      else ();
      let list =
        List.fold
          current_chalice_state.chalice_pickers
          ~init:[]
          ~f:(fun accum (c, p) ->
          if not (String.equal (Game.get_ip_address c) client_ip)
          then accum @ [ c, p ]
          else accum)
      in
      current_chalice_state.chalice_pickers <- list))
  else ();
  if List.is_empty current_chalice_state.chalice_pickers
  then chalice_ending coords game
  else return ()
;;

(* these are the people who decide which chalices to poison *)
let chalice_choosing client query (game : Game.t) =
  (let client_ip = Game.get_ip_address client in
   if List.exists current_chalice_state.chalice_choosers ~f:(fun (c, _) ->
        String.equal client_ip (Game.get_ip_address c))
   then (
     let chalice_number = Int.of_string query in
     if chalice_number > 0 && chalice_number < 5
     then (
       Chalices.poison_chalice chalice_number current_chalice_state;
       let list =
         List.fold
           current_chalice_state.chalice_choosers
           ~init:[]
           ~f:(fun accum (c, p) ->
           if not (String.equal (Game.get_ip_address c) client_ip)
           then accum @ [ c, p ]
           else accum)
       in
       current_chalice_state.chalice_choosers <- list);
     if List.is_empty current_chalice_state.chalice_choosers
     then (
       game.game_type <- Chalices true;
       let coords = display_players current_chalice_state.chalice_pickers in
       display_chalice_title_for_endangered_players ())
     else ())
   else ())
  |> return
;;

(* this is the function that starts the instructions for the Chalices
   minigame *)
let start_chalices_intro ~participants ~safe_players ~(game : Game.t)
  : unit Deferred.t
  =
  current_chalice_state.chalice_choosers <- safe_players;
  current_chalice_state.chalice_pickers <- participants;
  current_chalice_state.chalice_poisoned <- Array.create ~len:4 false;
  if List.is_empty current_chalice_state.chalice_choosers
     (* if there are no safe placers, then we will randomly pick 2 chalices
        (the same chalice may be picked twice )*)
  then (
    let first_chalice = Random.int 4 in
    let second_chalice = Random.int 4 in
    Chalices.poison_chalice first_chalice current_chalice_state;
    Chalices.poison_chalice second_chalice current_chalice_state;
    game.game_type <- Chalices true)
  else ();
  let span = Time_ns.Span.of_sec 6.0 in
  Clock_ns.run_after
    span
    (fun () ->
      if not (List.is_empty current_chalice_state.chalice_choosers)
      then display_chalice_title_for_safe_players ()
      else (
        let d = display_players current_chalice_state.chalice_pickers in
        display_chalice_title_for_endangered_players ()))
    ();
  display_chalice_title ()
;;

(* this shows the display_passwords of the players who have created
   password *)
let display_player_passwords () =
  Graphics.set_color Color.black;
  Graphics.fill_rect 500 250 600 400;
  Graphics.set_color Color.dark_red;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--60-*-*-*-*-*-iso8859-1";
  List.iter
    current_pp_state.active_participants
    ~f:(fun (c, p, real_pw, display_pw, i) ->
    Graphics.moveto i (player_y_coord - 100);
    Graphics.draw_string display_pw)
;;

(* this is the title screen for Password Pain *)
let display_pp_title () =
  Graphics.set_color Color.black;
  Graphics.fill_rect 0 0 1500 800;
  Graphics.set_color Color.pink;
  Graphics.fill_rect 500 250 500 300;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--60-*-*-*-*-*-iso8859-1";
  Graphics.set_color Color.black;
  Graphics.moveto 575 375;
  Graphics.draw_string "Password Pain"
;;

(* these are the directions for the participants of the password pain
   minigame*)
let display_pp_participant_instructions participants =
  let playing_players = display_players participants in
  Graphics.set_color Color.pink;
  Graphics.fill_rect 500 250 500 300;
  Graphics.set_color Color.black;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.moveto 635 400;
  Graphics.draw_string "Input a 4";
  Graphics.moveto 535 350;
  Graphics.draw_string "letter password";
  (* fixed the format for the record field *)
  current_pp_state.active_participants
    <- List.map playing_players ~f:(fun ((c, p), i) -> c, p, "", "", i)
;;

(* this starts the intro instructions for the password pain minigame *)

let start_pp_intro ~participants ~safe_players =
  current_pp_state.active_participants <- [];
  current_pp_state.safe_players <- [];
  current_pp_state.all_passwords_guessed <- false;
  if List.is_empty safe_players
  then
    (* no safe players means that every player got the question wrong - so
       everyone can guess each others passwords EXCEPT their own *)
    current_pp_state.safe_players <- participants
  else current_pp_state.safe_players <- safe_players;
  display_pp_title ();
  let span = Time_ns.Span.of_sec 3.0 in
  Clock_ns.run_after
    span
    (fun () -> display_pp_participant_instructions participants)
    ()
;;

(* this ends the minigame for password pain *)
let pp_end_minigame game =
  display_losers
    (List.filter
       current_pp_state.active_participants
       ~f:(fun (_, pl, _, _, _) -> not pl.living)
     |> List.map ~f:(fun (c, pl, _, _, x_coord) -> (c, pl), x_coord));
  let span = Time_ns.Span.of_sec 3.0 in
  let%bind () = Clock_ns.after span in
  create_leaderboard_graphics game
;;

(* these are the final set of instructions for the Password Pain minigame *)
let final_pp_instruction game =
  Graphics.set_color Color.pink;
  Graphics.fill_rect 500 250 500 300;
  Graphics.set_color Color.black;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.moveto 635 400;
  Graphics.draw_string "If your code ";
  Graphics.moveto 525 350;
  Graphics.draw_string "is guessed, you die";
  let span = Time_ns.Span.of_sec 4.0 in
  Clock_ns.run_after span (fun () -> display_player_passwords ()) ();
  (* after we display the initial passwords, we will set a timer before the
     game runs out*)
  let span = Time_ns.Span.of_sec 60.0 in
  (* we use the timer to call the minigame function if ALL the passwords are
     not already guessed *)
  let%bind () = Clock_ns.after span in
  if not current_pp_state.all_passwords_guessed
  then pp_end_minigame game
  else return ()
;;

(* these are the instructions for the safe players for Password Pain *)
let display_pp_safe_player_instructions game =
  Graphics.set_color Color.pink;
  Graphics.fill_rect 500 250 500 300;
  Graphics.set_color Color.black;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.moveto 625 420;
  Graphics.draw_string "Safe players";
  Graphics.moveto 535 350;
  Graphics.draw_string "Guess the password";
  let span = Time_ns.Span.of_sec 3.0 in
  let%bind () = Clock_ns.after span in
  final_pp_instruction game
;;

(* participant clients input the password that the safe players must guess *)
let pp_password_creation client query (game : Game.t) =
  let client_ip = Game.get_ip_address client in
  (* ensures that only players who got question wrong create passwords *)
  if List.exists
       current_pp_state.active_participants
       ~f:(fun (c, _, _, _, _) ->
       String.equal client_ip (Game.get_ip_address c))
  then
    if String.length query <> 4
    then return ()
    else (
      Password_pain.update_password client_ip current_pp_state ~query;
      (* that every password is a non empty string - all participating
         players put in their password *)
      if List.for_all
           current_pp_state.active_participants
           ~f:(fun (_, _, real_pw, display_pw, _) ->
           not (String.is_empty real_pw))
      then (
        game.game_type <- Password_pain true;
        display_pp_safe_player_instructions game)
      else return ())
  else return ()
;;

(* when the safe players guess the answer, this is what handles it *)
let pp_guesses client query (game : Game.t) =
  let client_ip = Game.get_ip_address client in
  (* we only accept guesses that are from safe players and are 4 letters
     long *)
  if String.length query = 4
     && List.exists current_pp_state.safe_players ~f:(fun (c, _) ->
          String.equal client_ip (Game.get_ip_address c))
  then (
    Password_pain.check_guess client_ip current_pp_state query game;
    display_player_passwords ());
  let span = Time_ns.Span.of_sec 1.0 in
  let%bind () = Clock_ns.after span in
  if current_pp_state.all_passwords_guessed
  then pp_end_minigame game
  else return ()
;;
