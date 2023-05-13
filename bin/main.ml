open Monopoly
open Raylib

open Player
(** [play_game f] starts the adventure in file [f]. *)

exception EndGame

let gui_flag = false
let board : Position.t = Position.new_board

type flow =
  | Play
  | End

type pset = {
  p1 : Player.t;
  p2 : Player.t;
}

let p1_col = Raylib.Color.blue
let p2_col = Raylib.Color.red

let rec card lst n m p =
  if n >= 0 then
    match lst with
    | [] ->
        ANSITerminal.print_string [ ANSITerminal.cyan ] m;
        p
    | (mes, pos) :: t -> card t (n - 1) mes pos
  else (
    print_endline m;
    p)

let command_txt txt = ANSITerminal.print_string [ ANSITerminal.red ] txt

(** [turn_actions flow player1 player2 proll] takes into account turn actions
    such as purchasing properties or paying taxes, [flow] determines whether or
    not the game has ended, [player1] is the player taking their turn, [player2]
    is the opposing player, and [proll] is the number rolled on the previous
    dice.*)
let rec turn_actions (flow : flow) (player1 : Player.t) (player2 : Player.t)
    (proll : int) : pset =
  try
    if flow = End then { p1 = player1; p2 = player2 }
    else
      let location = Player.current_location player1 in
      match location with
      | Position.Start data ->
          print_endline "You passed GO! Collect $200.";
          let play1 = Player.deposit 200 player1 in
          if gui_flag then Gui.draw_player_window play1 player2;
          { p1 = play1; p2 = player2 }
      | Position.Property data ->
          let owned =
            Player.tile_owned player1 location
            || Player.tile_owned player2 location
          in
          if owned then (
            print_endline (Position.get_name location ^ " is already owned");
            if Player.tile_owned player2 location then (
              let play1 = withdraw data.rent player1 in
              let play2 = deposit data.rent player2 in
              (* Pay rent *)
              print_endline
                (Player.get_name player2 ^ " owns " ^ Position.get_name location
               ^ ". Pay " ^ Player.get_name player2 ^ " "
               ^ string_of_int data.rent ^ ".");
              if gui_flag then Gui.draw_player_window play1 play2;
              { p1 = play1; p2 = play2 })
            else (
              (* Player1 owns it. *)
              print_endline ("You own " ^ Position.get_name location ^ ".");
              if gui_flag then Gui.draw_player_window player1 player2;
              { p1 = player1; p2 = player2 }))
          else (
            print_string
              (Position.get_name location ^ " is not owned. Would you like to ");
            command_txt "purchase";
            print_string " it or ";
            command_txt "end";
            print_endline " your turn?";
            match read_line () with
            | str -> (
                let command = Command.parse str in
                match command with
                | Command.Purchase ->
                    if owned = false then (
                      print_endline
                        ("You have purchased " ^ Position.get_name location
                       ^ "!");
                      let play1 = Player.buy_property location player1 in
                      if gui_flag then Gui.draw_player_window play1 player2;
                      {
                        p1 = Player.buy_property location player1;
                        p2 = player2;
                      })
                    else (
                      print_endline
                        (Position.get_name location
                       ^ " is already owned and cannot be purchased.");
                      if gui_flag then Gui.draw_player_window player1 player2;
                      { p1 = player1; p2 = player2 })
                | Command.Roll ->
                    print_endline
                      "You have already rolled this turn and cannot roll again";
                    turn_actions flow player1 player2 proll
                | Command.EndTurn ->
                    print_endline
                      ("You did not purchase " ^ Position.get_name location
                     ^ ". Your turn has ended.");
                    { p1 = player1; p2 = player2 }
                | Command.Quit ->
                    if gui_flag then Gui.draw_exit ();
                    raise EndGame))
      | Position.Railroad data ->
          let owned =
            Player.tile_owned player1 location
            || Player.tile_owned player2 location
          in
          if Player.tile_owned player2 location then (
            (*player 1 pays 2 * the number of rails owned*)
            let exch = 2 * Player.rails_owned board player2 in
            print_endline
              (Player.get_name player2 ^ " owns " ^ Position.get_name location
             ^ ". Pay " ^ Player.get_name player2 ^ " " ^ string_of_int exch
             ^ ".");
            let play1 = withdraw exch player1 in
            let play2 = deposit exch player2 in
            if gui_flag then Gui.draw_player_window play1 play2;
            { p1 = play1; p2 = play2 })
          else if Player.tile_owned player1 location then (
            print_endline
              (Player.get_name player1 ^ " owns " ^ Position.get_name location
             ^ ".");
            if gui_flag then Gui.draw_player_window player1 player2;
            { p1 = player1; p2 = player2 })
          else (
            print_string
              (Position.get_name location ^ " is not owned. Would you like to ");
            command_txt "purchase";
            print_string " it or ";
            command_txt "end";
            print_endline " your turn?";
            match read_line () with
            | str -> (
                let command = Command.parse str in
                match command with
                | Command.Purchase ->
                    if owned = false then (
                      print_endline
                        ("You have purchased " ^ Position.get_name location
                       ^ "!");
                      let play1 = Player.buy_property location player1 in
                      if gui_flag then Gui.draw_player_window play1 player2;
                      {
                        p1 = Player.buy_property location player1;
                        p2 = player2;
                      })
                    else (
                      print_endline
                        (Position.get_name location
                       ^ " is already owned and cannot be purchased.");
                      if gui_flag then Gui.draw_player_window player1 player2;
                      { p1 = player1; p2 = player2 })
                | Command.Roll ->
                    print_endline
                      "You have already rolled this turn and cannot roll again";
                    if gui_flag then Gui.draw_player_window player1 player2;
                    turn_actions flow player1 player2 proll
                | Command.EndTurn ->
                    print_endline
                      ("You did not purchase " ^ Position.get_name location
                     ^ ".");
                    { p1 = player1; p2 = player2 }
                | Command.Quit ->
                    if gui_flag then Gui.draw_exit ();
                    raise EndGame))
      | Position.Utility data ->
          let owned =
            Player.tile_owned player2 location
            || Player.tile_owned player1 location
          in
          if Player.tile_owned player2 location then (
            (*player 1 pays 2 * the number of rails owned* the number of
              utilities owned by the opposing player*)
            print_endline
              (Player.get_name player2 ^ " owns " ^ Position.get_name location
             ^ ".");
            let exch = 4 * proll * Player.util_owned board player2 in
            {
              p1 = Player.withdraw exch player1;
              p2 = Player.deposit exch player2;
            })
          else if Player.tile_owned player1 location then (
            print_endline
              (Player.get_name player1 ^ " owns " ^ Position.get_name location
             ^ ".");
            if gui_flag then Gui.draw_player_window player1 player2;
            { p1 = player1; p2 = player2 })
          else (
            print_string
              (Position.get_name location ^ " is not owned. Would you like to ");
            command_txt "purchase";
            print_string " it or ";
            command_txt "end";
            print_endline " your turn?";
            match read_line () with
            | str -> (
                let command = Command.parse str in
                match command with
                | Command.Purchase ->
                    if owned = false then (
                      print_endline
                        ("You have purchased " ^ Position.get_name location
                       ^ "!");
                      let play1 = Player.buy_property location player1 in
                      if gui_flag then Gui.draw_player_window play1 player2;
                      { p1 = play1; p2 = player2 })
                    else (
                      print_endline
                        (Position.get_name location
                       ^ " is already owned and cannot be purchased.");
                      if gui_flag then Gui.draw_player_window player1 player2;
                      { p1 = player1; p2 = player2 })
                | Command.Roll ->
                    print_endline
                      "You have already rolled this turn and cannot roll again";
                    if gui_flag then Gui.draw_player_window player1 player2;
                    turn_actions flow player1 player2 proll
                | Command.EndTurn ->
                    print_endline
                      ("You did not purchase " ^ Position.get_name location
                     ^ ".");
                    { p1 = player1; p2 = player2 }
                | Command.Quit ->
                    if gui_flag then Gui.draw_exit ();
                    raise EndGame))
      | Position.Rent data ->
          (*if gui_flag then Gui.draw_player_window player1 player2;*)
          raise
            (Failure
               "Unimplemented actions when a player lands on a Rent, line ~129 \
                in bin.main/ml")
      | Position.Jail data ->
          let play1 = Player.go_to_jail player1 in
          if gui_flag then Gui.draw_player_window play1 player2;
          { p1 = play1; p2 = player2 }
      | Position.Go_To_Jail data ->
          (*if gui_flag then Gui.draw_player_window play1 player2;*)
          let npos = 10 in
          let play1 =
            Player.move
              (npos - Position.get_index (current_location player1))
              board player1
          in
          if gui_flag then Gui.draw_player_window play1 player2;
          { p1 = play1; p2 = player2 }
      | Position.Chance data ->
          Random.self_init ();
          let r = Random.int 5 in
          let npos = card Position.chance_list r "" 0 in
          let play1 =
            Player.move
              (npos - Position.get_index (current_location player1))
              board player1
          in
          if gui_flag then Gui.draw_player_window play1 player2;
          turn_actions flow play1 player2 proll
      | Position.Community_Chest data ->
          Random.self_init ();
          let r = Random.int 5 in
          let nval = card Position.community_list r "" 0 in
          if nval >= 0 then (
            let uplay1 = Player.deposit nval player1 in
            print_endline
              ("Your new balance is " ^ string_of_int (Player.account uplay1));
            if gui_flag then Gui.draw_player_window uplay1 player2;
            { p1 = uplay1; p2 = player2 })
          else
            let uplay2 = Player.deposit (-nval) player1 in
            print_endline
              ("Your new balance is " ^ string_of_int (Player.account uplay2));
            if gui_flag then Gui.draw_player_window uplay2 player2;
            { p1 = uplay2; p2 = player2 }
      | Position.Free_Parking data -> { p1 = player1; p2 = player2 }
      | Position.Tax data ->
          print_endline
            ("You have paid " ^ string_of_int data.cost ^ " to taxes.");
          let play1 = withdraw data.cost player1 in
          if gui_flag then Gui.draw_player_window play1 player2;
          { p1 = play1; p2 = player2 }
  with
  | Command.Empty ->
      print_endline "Please enter a nonempty command.";
      turn_actions Play player1 player2 proll
  | Command.Malformed ->
      print_endline "Please enter a valid command.";
      turn_actions Play player1 player2 proll
  | Player.ExpensiveProperty ->
      print_endline
        "This property is too expensive to purchase. Your turn has been ended \
         and the property remains unowned.";
      { p1 = player1; p2 = player2 }
  | Player.Broke ->
      if gui_flag then Gui.draw_exit ();
      print_endline
        (Player.get_name player1 ^ " has lost and " ^ Player.get_name player2
       ^ " has won.");
      raise EndGame

(** [player state adv flow] parses the commands of the user into an action of
    the player. *)
let rec player (flow : flow) (player1 : Player.t) (player2 : Player.t) : unit =
  try
    print_endline "\n|==Bank Accounts==========|\n|";
    print_endline
      ("|  " ^ Player.get_name player1 ^ " has $"
      ^ string_of_int (Player.account player1));
    print_endline
      ("|  " ^ Player.get_name player2 ^ " has $"
      ^ string_of_int (Player.account player2));
    print_endline "|\n|=========================|";
    print_string
      ("\nIt is your turn, " ^ Player.get_name player1
     ^ "!\nPlease enter a command ");
    command_txt "roll";
    print_string " or ";
    command_txt "quit.\n";
    if gui_flag then Gui.draw_player_window player1 player2;
    if flow = Play then
      match read_line () with
      | str -> (
          let command = Command.parse str in
          match command with
          | Command.Quit ->
              if gui_flag then Gui.draw_exit ();
              print_endline "The game has ended. Thanks for playing!";
              player End player1 player2
          | Command.Roll ->
              let x = Random.int 10 + 2 in
              (* [nplay] is Player1 with new position *)
              let nplay = Player.move x board player1 in
              print_endline
                ("You rolled a " ^ string_of_int x ^ " and have landed on "
                ^ Position.get_name (Player.current_location nplay));
              (* draw the new position *)
              if gui_flag then Gui.draw_player_window nplay player2;
              let fplay = turn_actions flow nplay player2 x in
              (* if gui_flag then Gui.draw_player_window player1 player 2
                 p1_col;*)
              player Play fplay.p2 fplay.p1
          | Command.Purchase ->
              print_endline "You cannot make a property purchase at this time";
              player Play player1 player2
          | Command.EndTurn ->
              print_endline "Your turn has ended.";
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
  (*if gui_flag then Gui.setup (); Gui.loop();*)
  if gui_flag then Gui.setup ();
  if gui_flag then Gui.draw_intro ();
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to MONOPOLY! \n\n";
  print_endline "General Rules :";
  print_string
    "Turns \
     ---------------------------------------------------------------------- \n\
     |\n\
     | Your name will be called for your turn. \n\
     | At the beginning of your turn, you can roll the dice and move using\n\
     | ";
  command_txt "roll";
  print_string " or quit the game using ";
  command_txt "quit";
  print_string "\n| \n";
  print_string
    "Commands ------------------------------------------------------------------\n\
     |\n\
     | Once you land on a square, you can be prompted to perform \"commands\" \n\
     | such as \n\
     | ";
  command_txt "purchase";
  print_endline
    " to buy and own a property. Players will pay you rent when \n\
     | they land on this property in the future.\n\
     |";
  print_string
    "End Turn / Game \
     ----------------------------------------------------------- \n\
     |\n\
     | At any point you may ";
  command_txt "end";
  print_string " your turn or ";
  command_txt "quit";
  print_endline " the game.\n|";
  print_endline "\nLet's Play!\n";
  print_endline "Player one, please enter your name: \n";
  print_string "> ";
  let name1 =
    match read_line () with
    | x -> x
  in
  print_endline "Player two, please enter your name: \n";
  print_string "> ";
  let name2 =
    match read_line () with
    | x -> x
  in
  (*if gui_flag then Gui.draw_player_window (Player.new_player name1);*)
  player Play (Player.new_player name1 p1_col) (Player.new_player name2 p2_col)

(* Execute the game engine. *)
let () = main ()

(*old version of purchasing commands, please do not delete :

  let title = String.concat " " comm_lst in let nplay = Player.buy_property
  title player1 in print_endline (" You have purchased " ^ title); player Play
  player2 nplay board*)
