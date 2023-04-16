open Monopoly
(* open Raylib *)

open Player
(** [play_game f] starts the adventure in file [f]. *)

type flow =
  | Play
  | End

(** [player state adv flow] parses the commands of the user into an action of
    the player. *)
let rec player flow play name =
  try
    print_endline
      ("It is your turn, " ^ name ^ "!\nPlease enter a command (roll or quit).");
    if flow = Play then
      match read_line () with
      | str -> (
          let command = Command.parse str in
          match command with
          | Command.Quit ->
              print_endline "The game has ended. Thanks for playing!";
              player End play name
          | Command.Roll ->
              let x = Random.int 10 + 2 in
              let nplay = Player.move x play in
              print_endline
                ("You rolled a " ^ string_of_int x ^ " and have landed on "
                ^ Position.get_name (Board.position (Player.get_board nplay)));
              player Play nplay name
              (* let position_type *)
          | Command.Purchase comm_lst ->
              let title = String.concat " " comm_lst in
              let nplay = Player.buy_property title play in
              print_endline (" You have purchased " ^ title);
              player Play nplay name)
    else print_endline "The game has ended. Thanks for playing!"
  with
  | Command.Empty ->
      print_endline "Please enter a nonempty command.";
      player Play play name
  | Command.Malformed ->
      print_endline "Please enter a valid command.";
      player Play play name

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  (* Gui.setup () |> Gui.loop; *)
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to MONOPOLY! \n";
  print_endline "Please enter your name: \n";
  print_string "> ";
  match read_line () with
  | x ->
      player Play (Player.new_player x) x;
      ()

(* Execute the game engine. *)
let () = main ()
