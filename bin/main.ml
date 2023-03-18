open Monopoly

open Player
(** [play_game f] starts the adventure in file [f]. *)

let play_game f = raise (Failure "Unimplemented: Main.play_game")
let data_dir_prefix = "data" ^ Filename.dir_sep



type flow =
|Play
|End

(** [player state adv flow] parses the commands of the user into an action of the player. *)
let rec player flow play=
  print_endline("Please enter a command.");
  if flow = Play then
    begin
    match read_line() with 
    |str -> let command = Command.parse str in
    match command with
    |Command.Quit -> print_endline("the game has ended."); player End play
    |Command.Roll -> print_endline("dice rolled");  player Play (Player.move_to play)
    end
  else print_endline("the game has ended"); player End play


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to MONOPOLY! \n";
  print_endline "Please enter your name: \n";
  print_string "> ";
  match read_line () with
  | x -> player Play (Player.new_player x); ()
(* Execute the game engine. *)
let () = main ()
