open Monopoly
(* open Raylib *)

open Player
(** [play_game f] starts the adventure in file [f]. *)

type flow =
  | Play
  | End

(** [player state adv flow] parses the commands of the user into an action of
    the player. *)
let rec player (flow : flow) (player1 : Player.t) (player2 : Player.t)
    (board : Position.t) =
  try
    print_endline
      ("It is your turn, " ^ Player.get_name player1
     ^ "!\nPlease enter a command (roll or quit).");
    if flow = Play then
      match read_line () with
      | str -> (
          let command = Command.parse str in
          match command with
          | Command.Quit ->
              print_endline "The game has ended. Thanks for playing!";
              player End player1 player2 board
          | Command.Roll ->
              let x = Random.int 10 + 2 in
              (* [nplay] is Player1 with new position *)
              let nplay = Player.move x board player1 in
              print_endline
                ("You rolled a " ^ string_of_int x ^ " and have landed on "
                ^ Position.get_name (Player.current_location nplay));
              player Play player2 nplay board
          | Command.Purchase comm_lst ->
              let title = String.concat " " comm_lst in
              let nplay = Player.buy_property title player1 in
              print_endline (" You have purchased " ^ title);
              player Play player2 nplay board)
    else print_endline "The game has ended. Thanks for playing!"
  with
  | Command.Empty ->
      print_endline "Please enter a nonempty command.";
      player Play player1 player2 board
  | Command.Malformed ->
      print_endline "Please enter a valid command.";
      player Play player1 player2 board

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  (* Gui.setup () |> Gui.loop; *)
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to MONOPOLY! \n";
  print_endline "Player1, please enter your name: \n";
  print_string "> ";
  let name1 =
    match read_line () with
    | x -> x
  in
  print_endline "Player2, please enter your name: \n";
  print_string "> ";
  let name2 =
    match read_line () with
    | x -> x
  in
  player Play (Player.new_player name1) (Player.new_player name2)
    Position.new_board

(* Execute the game engine. *)
let () = main ()
