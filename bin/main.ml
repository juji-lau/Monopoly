open Monopoly

open Player
(** [play_game f] starts the adventure in file [f]. *)

type flow =
|Play
|End

(** [player state adv flow] parses the commands of the user into an action of the player. *)
let rec player flow play=
try
  print_endline("Please enter a command.");
  print_string "> ";
  if flow = Play then
    begin
    match read_line() with 
    |str -> let command = Command.parse str in
    match command with
    |Command.Quit -> print_endline("The game has ended."); player End play
    |Command.Roll -> let nplay = Player.move play in 
    print_endline("Dice rolled. New position is: " ^ (string_of_int (Player.current_location nplay))); 
     player Play nplay
    end
  else print_endline("the game has ended") with
  |Command.Empty -> print_endline("Please enter a nonempty command."); player Play play
  |Command.Malformed -> print_endline("Please enter a valid command."); player Play play


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to MONOPOLY! \n";
  print_endline "Please enter your name: \n";
  print_string "> ";
  match read_line () with
  | x -> player Play (Player.new_player x); ()
(* Execute the game engine. *)
let () = main ()
