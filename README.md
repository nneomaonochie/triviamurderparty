# triviamurderparty
This is the JSIP Personal Project emulating the Jackbox game Trivia Murder Party - beyond the prototype.

If you are the server, input this into the terminal: 
dune exec bin/main.exe -- tmp-server server -port 8181

If you are the client, use the send-message feature to submit your name and participate in minigames such as Math Mayhem.
Use the send-char feature to submit your answer to the trivia games.
**localhost** should be used for when you have the same computer for the server and a client. 
Use the AWS box url of the server port to hook up a client from another computer 

Ex: dune exec bin/main.exe -- tmp-server client send-message -server localhost:8181 -message "name"
This sends a string to a server that is on the same device as the client

Ex: dune exec bin/main.exe -- tmp-server client send-char -server ec2-52-201-77-219.compute-1.amazonaws.com:8181 -char hello
This sends a char to a server that is on a different device from the client
