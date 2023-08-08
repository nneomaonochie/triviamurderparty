[@@@disable_unused_warnings]

open! Core
open! Async

(* for both width and heights of the players *)
let player_block_size = 125

(* needs to be reset each time Math_Mayhem minigame is run*)
let current_math_mayhem_hashtables = Math_mayhem.create ()
let current_pp_state = Password_pain.create ()

(* based off the number of players that are starting - index = numPlayers -
   1 *)
let player_starting_x_coords = [ 687; 550; 437; 300 ]
let player_y_coord = 620

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

let animation_test x y =
  Graphics.set_color Color.red;
  Graphics.fill_circle x y 50;
  Graphics.set_color Color.black;
  Graphics.fill_circle (x - 10) (y - 10) 50
;;

(* this asks user input for players' names so that we can initiatilize the
   players and create a game *)

let draw_chalices () =
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
  Graphics.draw_string "4"
;;

let display_chalice_instructions () =
  Graphics.set_color Color.pastel_purple;
  Graphics.fill_rect 500 250 500 300;
  Graphics.set_color Color.white;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.moveto 635 400;
  Graphics.draw_string "Chalices"
;;

let display_chalice_title () =
  Graphics.set_color Color.pastel_purple;
  Graphics.fill_rect 500 250 500 300;
  Graphics.set_color Color.white;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.moveto 635 400;
  Graphics.draw_string "Safe players, pick a chalice";
  Graphics.moveto 535 350;
  Graphics.draw_string "Choose wrong, you die"
;;

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
<<<<<<< HEAD
  Graphics.set_color Color.dark_red;
=======
  (* testing animation *)
  (*let stop_test = ref false in let coordinates = Array.create ~len:2 0 in
    every 0.25 ~f:(fun () -> animation_test coordinates.(0) coordinates.(1);
    Array.set coordinates 0 (coordinates.(0) + 10); Array.set coordinates 1
    (coordinates.(1) + 10); if coordinates.(1) > 800 then stop_test := true)
    ~stop:stop_test; *)
  (* testing animation again*)
>>>>>>> e19b6d6a6eaf425ac7304ef0ffff942b54a96227
  Graphics.draw_rect 375 400 600 200;
  Graphics.fill_rect 375 400 600 200;
  Graphics.moveto 400 500;
  Graphics.set_color Color.red;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  Graphics.draw_string "Please enter your name into the console";
  draw_chalices ()
;;

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
    (* if player looks dead, put an x *)
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

let display_ending_graphics (game : Game.t) =
  let display_alive_bonus () =
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
  in
  let display_winner () =
    Graphics.set_color Color.black;
    Graphics.fill_rect 0 0 1500 800;
    let winner = List.hd_exn game.player_list in
    let _, p = winner in
    let d = display_players [ winner ] in
    Graphics.set_color Color.green;
    Graphics.moveto 680 400;
    Graphics.draw_string "The Winner";
    ()
  in
  let span_of_alive_bonus = Time_ns.Span.of_sec 6.0 in
  Clock_ns.run_after
    span_of_alive_bonus
    (fun () -> display_alive_bonus ())
    ();
  let span_of_winner = Time_ns.Span.of_sec 6.0 in
  Clock_ns.run_after span_of_winner (fun () -> display_winner ()) ()
;;

(* these are the graphics for the final round *)
let display_final_round (game : Game.t) =
  Final_round.print_random_question ()
;;

let create_trivia_graphics (game : Game.t) =
  if List.for_all game.player_list ~f:(fun (_, p) -> not p.living)
     || game.questions_asked > 1
  then (
    game.game_state <- Game_over;
    display_ending_graphics game)
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
    Graphics.draw_string (List.nth_exn question.answer_choices 3))
;;

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
  Clock_ns.run_after span (fun () -> create_trivia_graphics game) ()
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
    (* find a way to display the time you have left [might be OPTIONAL]*)
    Clock_ns.run_after span (fun () -> create_leaderboard_graphics game) ())
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
    (* find a way to display the time you have left [might be OPTIONAL]*)
    Clock_ns.run_after span (fun () -> create_leaderboard_graphics game) ())
;;

(* displays the title of *)
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
  (* find a way to display the time you have left [might be OPTIONAL]*)
  Clock_ns.run_after span (fun () -> math_mayhem_calc_scores game) ()
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

let create_decision_graphics () = ()

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

(* this ends the minigame for password pain *)
let pp_end_minigame game =
  display_losers
    (List.filter
       current_pp_state.active_participants
       ~f:(fun (_, pl, _, _, _) -> not pl.living)
     |> List.map ~f:(fun (c, pl, _, _, x_coord) -> (c, pl), x_coord));
  let span = Time_ns.Span.of_sec 3.0 in
  Clock_ns.run_after span (fun () -> create_leaderboard_graphics game) ()
;;

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
  Clock_ns.run_after
    span
    (fun () ->
      if not current_pp_state.all_passwords_guessed then pp_end_minigame game)
    ()
;;

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
  Clock_ns.run_after span (fun () -> final_pp_instruction game) ()
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
    then print_s [%message "Your password must be 4 letters."]
    else (
      Password_pain.update_password client_ip current_pp_state ~query;
      (* that every password is a non empty string - all participating
         players put in their password *)
      if List.for_all
           current_pp_state.active_participants
           ~f:(fun (_, _, real_pw, display_pw, _) ->
           print_s [%message real_pw display_pw];
           not (String.is_empty real_pw))
      then (
        print_s
          [%message
            ""
              (current_pp_state.safe_players
                : (Socket.Address.Inet.t * Player.t) list)
              (current_pp_state.active_participants
                : (Socket.Address.Inet.t * Player.t * string * string * int)
                  list)];
        game.game_type <- Password_pain true;
        display_pp_safe_player_instructions game))
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
  Clock_ns.run_after
    span
    (fun () ->
      if current_pp_state.all_passwords_guessed then pp_end_minigame game)
    ()
;;
