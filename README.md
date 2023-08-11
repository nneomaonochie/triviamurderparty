## triviamurderparty
This is the JSIP Personal Project emulating the Jackbox game Trivia Murder Party - beyond the prototype. The game is intended for 4 players, but you can edit the number of players in the game.ml variable **max_players**. If allowing more than 4 players, adjust the player coordinates in the Tmp_graphics.ml. The game starts up and allows players to answer trivia questions. Players who answer incorrectly are dragged into playing the minigames, of which there are three, Math Mayhem, Password Pain, and Chalices. When there are less than two players remaining alive, players do a lightning round of questions where the first player to exit the screen wins.


# starting up the game
If you are the server, input this into the terminal: 
dune exec bin/main.exe -- tmp-server server -port 8181

If you are the client, use the send-message feature to submit your name and participate in minigames.
Use the send-char feature to submit your answer to the trivia games.
**localhost** should be used for when you have the same computer for the server and a client. 
Use the AWS box url of the server port to hook up a client from another computer 

Ex: dune exec bin/main.exe -- tmp-server client send-message -server localhost:8181 -message "name"
This sends a string to a server that is on the same device as the client

Ex: dune exec bin/main.exe -- tmp-server client send-char -server ec2-52-201-77-219.compute-1.amazonaws.com:8181 -char hello
This sends a char to a server that is on a different device from the client

# Bash scripts for input

For faster play time, we implemented bash scripts for both starting games and submitting user input to the server

To start a game, do **./start_game** 

To submit your name at the beginning of the, do **./name_program your_first_name**

To submit your answers for trivia questions, do **./trivia_input answer** where answer should be one of the characters Q, W, E, or R

To submit your answers for minigames, do **./minigame_input answer**

What you input for minigames will vary depending on which minigame you are playing

# Tmp_server
This module is used to create the server where clients can connect to the game. The main drivers are the **handle_query_string** function and the **handle_query_char** function, which take in queries from the client and use the user input to update the game state. Chars are used for the trivia questions and messages are used for name creation and minigame input 

# Minigame logic
Within the file, there is a Game module that keeps track of the current Game_state and Game_type. These record fields indicate when minigames are played, which minigames players will playing, and which queries are used to run the game at specific moments.

# Tmp_graphics
This is the main function that runs the graphics displayed on the screen. This project relies on the Ocaml Graphics library, and the game is run on the OCaml Graphics GUI. 
