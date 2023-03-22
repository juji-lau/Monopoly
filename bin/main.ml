open Monopoly

open Player
(** [play_game f] starts the adventure in file [f]. *)

type flow =
  | Play
  | End

(** [player state adv flow] parses the commands of the user into an action of
    the player. *)
let rec player flow play =
  try
    print_endline "Please enter a command (roll or quit).";
    print_string "> ";
    if flow = Play then
      match read_line () with
      | str -> (
          let command = Command.parse str in
          match command with
          | Command.Quit ->
              print_endline "The game has ended. Thanks for playing!";
              player End play
          | Command.Roll ->
              let x = Random.int 12 in
              let nplay = Player.move play x in
              print_endline
                ("You rolled a : " ^ string_of_int x ^ " New position is: "
                ^ Position.get_name (Board.position (Player.get_board nplay)));
              player Play nplay)
    else print_endline "The game has ended. Thanks for playing!"
  with
  | Command.Empty ->
      print_endline "Please enter a nonempty command.";
      player Play play
  | Command.Malformed ->
      print_endline "Please enter a valid command.";
      player Play play

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to MONOPOLY! \n";
  print_endline "Please enter your name: \n";
  print_string "> ";
  match read_line () with
  | x ->
      player Play (Player.new_player x);
      ()

(* Execute the game engine. *)
let () = main ()
