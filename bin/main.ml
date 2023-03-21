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
              let dice_roll = Random.int 10 + 2 in
              let nplay = Player.move play dice_roll in
              print_endline
                ("You rolled a " ^ string_of_int dice_roll
               ^ " and have landed on "
                ^
                match Player.current_location nplay with
                | 0 -> "Go"
                | 1 -> "Mediterranean Avenue"
                | 2 -> "a Community Chest"
                | 3 -> "Baltic Avenue"
                | 4 -> "Tax"
                | 5 -> "Reading Railroad"
                | 6 -> "Oriental Avenue"
                | 7 -> "Chance"
                | 8 -> "Vermont Avenue"
                | 9 -> "Connecticut Avenue"
                | 10 -> "Visiting Jail"
                | 11 -> "St. Charles Place"
                | 12 -> "Electric Company"
                | 13 -> "States Avenue"
                | 14 -> "Virginia Avenue"
                | 15 -> "Pennsylvania Railroad"
                | 16 -> "St. James Place"
                | 17 -> "a Community Chest"
                | 18 -> "Tennessee Avenue"
                | 19 -> "New York Avenue"
                | 20 -> "Free Parking"
                | 21 -> "Kentucky Avenue"
                | 22 -> "Chance"
                | 23 -> "Indiana Avenue"
                | 24 -> "Illinois Avenue"
                | 25 -> "B&O Railroad"
                | 26 -> "Atlantic Avenue"
                | 27 -> "Ventnor Avenue"
                | 28 -> "Water Works"
                | 29 -> "Marvin Gardens"
                | 30 -> "Go To Jail"
                | 31 -> "Pacific Avenue"
                | 32 -> "North Carolina Avenue"
                | 33 -> "a Community Chest"
                | 34 -> "Pennsylvania Avenue"
                | 35 -> "Short Line Railroad"
                | 36 -> "Chance"
                | 37 -> "Park Place"
                | 38 -> "Tax"
                | 39 -> "Boardwalk"
                | c -> "Another Dimension!");
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
