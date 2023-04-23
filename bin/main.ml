open Monopoly
(* open Raylib *)

open Player
(** [play_game f] starts the adventure in file [f]. *)

exception EndGame

type flow =
  | Play
  | End

(** [turn_actions flow player1 player2 board] takes into account turn actions
    such as purchasing properties or paying taxes*)
let rec turn_actions (flow : flow) (player1 : Player.t) (player2 : Player.t)
    (board : Position.t) : Player.t =
  if flow = End then player1
  else
    let location = Player.current_location player1 in
    match location with
    | Position.Start data ->
        print_endline "";
        Player.deposit 200 player1
    | Position.Property data ->
        let owned =
          Player.tile_owned player1 location
          || Player.tile_owned player2 location
        in
        if owned then (
          print_endline "This property is already owned";
          if Player.tile_owned player2 location then withdraw data.rent player1
          else player1)
        else (
          print_endline "This property is not owned and can be purchased.";
          match read_line () with
          | str -> (
              let command = Command.parse str in
              match command with
              | Command.Purchase ->
                  if owned = false then (
                    print_endline "You have purchased the current property.";
                    Player.buy_property location player1)
                  else (
                    print_endline
                      "This property is already owned and cannot be purchased.";
                    player1)
              | Command.Roll ->
                  print_endline
                    "You have already rolled this turn and cannot roll again";
                  turn_actions flow player1 player2 board
              | Command.EndTurn ->
                  print_endline "The property was not purchased";
                  player1
              | Command.Quit -> raise EndGame))
    | Position.Railroad data ->
        raise
          (Failure
             "Unimplemented actions when a player lands on Railroads, line 46 \
              in bin.main/ml")
    | Position.Utility data ->
        raise
          (Failure
             "Unimplemented actions when a player lands on a Utility, line ~55 \
              in bin.main/ml")
    | Position.Rent data ->
        raise
          (Failure
             "Unimplemented actions when a player lands on a Rent, line ~58 in \
              bin.main/ml")
    | Position.Jail data ->
        raise
          (Failure
             "Unimplemented actions when a player lands on a Jail, line ~63 in \
              bin.main/ml")
    | Position.Go_To_Jail data -> player1
    | Position.Chance data -> player1
    | Position.Community_Chest data -> player1
    | Position.Free_Parking data -> player1
    | Position.Tax data -> player1

(** [player state adv flow] parses the commands of the user into an action of
    the player. *)
let rec player (flow : flow) (player1 : Player.t) (player2 : Player.t)
    (board : Position.t) : unit =
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
              let fplay = turn_actions flow nplay player2 board in
              player Play player2 fplay board
          | Command.Purchase ->
              print_endline " You cannot make a property purchase at this time";
              player Play player1 player2 board
          | Command.EndTurn ->
              print_endline "Forfeitting turn to next player";
              player Play player2 player1 board)
    else print_endline "The game has ended. Thanks for playing!"
  with
  | Command.Empty ->
      print_endline "Please enter a nonempty command.";
      player Play player1 player2 board
  | Command.Malformed ->
      print_endline "Please enter a valid command.";
      player Play player1 player2 board
  | EndGame -> print_endline "The game has ended. Thank you for playing!"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  (* Gui.setup () |> Gui.loop; *)
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to MONOPOLY! \n";
  print_endline
    "General Rules : \n\n\
    \  At the beginning of your turn you can roll the dice and move using \
     \"roll\" \n\
    \  or quit the game using \"quit\" \n\n\
    \  Once you land on a purchasable property, you can purchase it using \
     \"purchase\" \n\
    \  or end your turn using \"end\" \n";
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

(*old version of purchasing commands, please do not delete :

  let title = String.concat " " comm_lst in let nplay = Player.buy_property
  title player1 in print_endline (" You have purchased " ^ title); player Play
  player2 nplay board*)
