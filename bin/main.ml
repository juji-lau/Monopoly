open Monopoly
(* open Raylib *)

open Player
(** [play_game f] starts the adventure in file [f]. *)

exception EndGame

let board : Position.t = Position.new_board

type flow =
  | Play
  | End

type pset = {
  p1 : Player.t;
  p2 : Player.t;
}

(** [turn_actions flow player1 player2 proll] takes into account turn actions
    such as purchasing properties or paying taxes, [flow] determines whether or
    not the game has ended, [player1] is the player taking their turn, [player2]
    is the opposing player, and [proll] is the number rolled on the previous
    dice.*)
let rec turn_actions (flow : flow) (player1 : Player.t) (player2 : Player.t)
    (proll : int) : pset =
  if flow = End then { p1 = player1; p2 = player2 }
  else
    let location = Player.current_location player1 in
    match location with
    | Position.Start data ->
        print_endline "";
        { p1 = Player.deposit 200 player1; p2 = player2 }
    | Position.Property data ->
        let owned =
          Player.tile_owned player1 location
          || Player.tile_owned player2 location
        in
        if owned then (
          print_endline "This property is already owned";
          if Player.tile_owned player2 location then
            { p1 = withdraw data.rent player1; p2 = deposit data.rent player2 }
          else { p1 = player1; p2 = player2 })
        else (
          print_endline "This property is not owned and can be purchased.";
          match read_line () with
          | str -> (
              let command = Command.parse str in
              match command with
              | Command.Purchase ->
                  if owned = false then (
                    print_endline "You have purchased the current property.";
                    { p1 = Player.buy_property location player1; p2 = player2 })
                  else (
                    print_endline
                      "This property is already owned and cannot be purchased.";
                    { p1 = player1; p2 = player2 })
              | Command.Roll ->
                  print_endline
                    "You have already rolled this turn and cannot roll again";
                  turn_actions flow player1 player2 proll
              | Command.EndTurn ->
                  print_endline "The property was not purchased";
                  { p1 = player1; p2 = player2 }
              | Command.Quit -> raise EndGame))
    | Position.Railroad data ->
        let owned =
          Player.tile_owned player1 location
          || Player.tile_owned player2 location
        in
        if Player.tile_owned player2 location then (
          (*player 1 pays 2 * the number of rails owned*)
          print_endline (Player.get_name player2 ^ "owns this property.");
          let exch = 2 * Player.rails_owned board player2 in
          { p1 = withdraw exch player1; p2 = deposit exch player2 })
        else if Player.tile_owned player1 location then (
          print_endline (Player.get_name player1 ^ "owns this property.");
          { p1 = player1; p2 = player2 })
        else (
          print_endline "This property is not owned and can be purchased.";
          match read_line () with
          | str -> (
              let command = Command.parse str in
              match command with
              | Command.Purchase ->
                  if owned = false then (
                    print_endline "You have purchased the current property.";
                    { p1 = Player.buy_property location player1; p2 = player2 })
                  else (
                    print_endline
                      "This property is already owned and cannot be purchased.";
                    { p1 = player1; p2 = player2 })
              | Command.Roll ->
                  print_endline
                    "You have already rolled this turn and cannot roll again";
                  turn_actions flow player1 player2 proll
              | Command.EndTurn ->
                  print_endline "The property was not purchased";
                  { p1 = player1; p2 = player2 }
              | Command.Quit -> raise EndGame))
    | Position.Utility data ->
        let owned =
          Player.tile_owned player2 location
          || Player.tile_owned player1 location
        in
        if Player.tile_owned player2 location then (
          (*player 1 pays 2 * the number of rails owned* the number of utilities
            owned by the opposing player*)
          print_endline (Player.get_name player2 ^ "owns this property.");
          let exch = 4 * proll * Player.util_owned board player2 in
          {
            p1 = Player.withdraw exch player1;
            p2 = Player.deposit exch player2;
          })
        else if Player.tile_owned player1 location then (
          print_endline (Player.get_name player1 ^ "owns this property.");
          { p1 = player1; p2 = player2 })
        else (
          print_endline "This property is not owned and can be purchased.";
          match read_line () with
          | str -> (
              let command = Command.parse str in
              match command with
              | Command.Purchase ->
                  if owned = false then (
                    print_endline "You have purchased the current property.";
                    { p1 = Player.buy_property location player1; p2 = player2 })
                  else (
                    print_endline
                      "This property is already owned and cannot be purchased.";
                    { p1 = player1; p2 = player2 })
              | Command.Roll ->
                  print_endline
                    "You have already rolled this turn and cannot roll again";
                  turn_actions flow player1 player2 proll
              | Command.EndTurn ->
                  print_endline "The property was not purchased";
                  { p1 = player1; p2 = player2 }
              | Command.Quit -> raise EndGame))
    | Position.Rent data ->
        raise
          (Failure
             "Unimplemented actions when a player lands on a Rent, line ~129 \
              in bin.main/ml")
    | Position.Jail data ->
        raise
          (Failure
             "Unimplemented actions when a player lands on a Jail, line ~134 \
              in bin.main/ml")
    | Position.Go_To_Jail data ->
        raise
          (Failure "Unimplemented Go to Jail Square, line ~139 in bin/main.ml")
    | Position.Chance data ->
        raise (Failure "Unimplemented Chance Square, line ~139 in bin/main.ml")
    | Position.Community_Chest data ->
        raise
          (Failure
             "Unimplemented Community Chest Square, line ~139 in bin/main.ml")
    | Position.Free_Parking data -> { p1 = player1; p2 = player2 }
    | Position.Tax data -> { p1 = withdraw data.cost player1; p2 = player2 }

(** [player state adv flow] parses the commands of the user into an action of
    the player. *)
let rec player (flow : flow) (player1 : Player.t) (player2 : Player.t) : unit =
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
              player End player1 player2
          | Command.Roll ->
              let x = Random.int 10 + 2 in
              (* [nplay] is Player1 with new position *)
              let nplay = Player.move x board player1 in
              print_endline
                ("You rolled a " ^ string_of_int x ^ " and have landed on "
                ^ Position.get_name (Player.current_location nplay));
              let fplay = turn_actions flow nplay player2 x in
              player Play fplay.p2 fplay.p1
          | Command.Purchase ->
              print_endline " You cannot make a property purchase at this time";
              player Play player1 player2
          | Command.EndTurn ->
              print_endline "Forfeitting turn to next player";
              player Play player2 player1)
    else print_endline "The game has ended. Thanks for playing!"
  with
  | Command.Empty ->
      print_endline "Please enter a nonempty command.";
      player Play player1 player2
  | Command.Malformed ->
      print_endline "Please enter a valid command.";
      player Play player1 player2
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

(* Execute the game engine. *)
let () = main ()

(*old version of purchasing commands, please do not delete :

  let title = String.concat " " comm_lst in let nplay = Player.buy_property
  title player1 in print_endline (" You have purchased " ^ title); player Play
  player2 nplay board*)
