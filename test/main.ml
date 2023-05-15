open OUnit2
open Monopoly
open Player
open Position
open Account
<<<<<<< HEAD
open Command

(* Test Plan - Every function is included in an OUnit test case in this suite
   excluding functions related to the GUI - Outputs in the GUI and terminal were
   manually tested by extensively playing the game - Lines of code are 95%
   covered (shown from bisect) and mostly excludes errors that should not
   actually occur in the flow of the program - OUnit test cases were first
   developed by black-box testing where someone who didn't implement the
   function tested it based only on looking at the specifications of the
   function. In this way, the tester was also able to make clarifications in the
   mli files about the functionalities. - Glass Box testing was also used when
   trying to improve the coverage of our test cases and we made extra test cases
   to cover code that bisect showed us was untested - We also chose random
   properties to test in unit testing while making sure each type of square on
   the board is tested - This system is thus quite thoroughly tested through
   both OUnit tests covering the lines of code as well as manually playing the
   game until every square has been landed on by the user to check its
   functionality even including testing that error messages are shown where
   needed*)

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

let pp_int s = string_of_int s

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(* =============== Player tests: ================ *)
(* make a board *)
let board = Position.new_board

let print_position position =
  "square name: " ^ get_name position ^ " at position: "
  ^ string_of_int (get_index position)

(* helper function to get the square from the name *)
let get_square_from_name pos = Position.square_name board pos

(* grab a few squares: *)
let baltic = get_square_from_name "Baltic Avenue"
let oriental = get_square_from_name "Oriental Avenue"
let james = get_square_from_name "St. James Place"
let tennessee = get_square_from_name "Tennessee Avenue"
let ventnor = get_square_from_name "Ventnor Avenue"
let kentucky = get_square_from_name "Kentucky Avenue"
let indiana = get_square_from_name "Indiana Avenue"
let illinois = get_square_from_name "Illinois Avenue"
let marvin = get_square_from_name "Marvin Gardens"
let boardwalk = get_square_from_name "Boardwalk"
let reading_railroad = get_square_from_name "Reading Railroad"
let water_works = get_square_from_name "Water Works"
let start = get_square_from_name "Go"
let bo_railroad = get_square_from_name "B&O Railroad"
let electric_company = get_square_from_name "Electric Company"
let tax = get_square_from_name "pay Tax"

(* make a few players at various locations, with various properties: *)
let alexandra0 = new_player "Alexandra" Raylib.Color.blue

let brooke21 =
  move 21 Position.new_board (new_player "Brooke" Raylib.Color.green)

let juji35 = move 35 Position.new_board (new_player "Juji" Raylib.Color.red)

let sophie37 =
  move 37 Position.new_board (new_player "Sophie" Raylib.Color.yellow)

(* Tests that [new_player] is initialized to the correct values using [get_name]
   [account] [current_location], [get_owned_properties], and [get_color]*)
let new_player_test (name : string) (player_name : string) (player : Player.t)
    (color : Raylib.Color.t) : test =
  name >:: fun _ ->
  assert_equal player_name (Player.get_name player);
  assert_equal
    (Position.get_initial Position.new_board)
    (current_location player);
  assert_equal [] (get_owned_properties player);
  assert_equal color (Player.get_color player)

(* Tests [move] after the player has moved [x] spots, using [current_location]*)
let move_test (name : string) (person : Player.t) (x : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (move x board person |> current_location |> get_index)
    ~printer:string_of_int

(*let move_test_debug (name : string) (person : Player.t) (x : int)
  (expected_output : Position.square) : test = name >:: fun _ -> assert_equal
  expected_output (move x board person |> current_location)
  ~printer:print_position*)

(* Give the players properties *)
let brookeboard = buy_property boardwalk brooke21

let jujired =
  buy_property kentucky juji35 |> buy_property indiana |> buy_property illinois

let sophierich =
  buy_property baltic sophie37
  |> buy_property oriental |> buy_property james |> buy_property marvin

(* Tests [get_owned_properites] *)
let get_owned_properties_test (name : string) (person : Player.t)
    (expected_output : Position.square list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_owned_properties person)
    ~printer:(pp_list print_position)

(* Tests [buy_property] using [get_owned_properties] *)
let buy_property_test (name : string) (person : Player.t)
    (property : Position.square) (expected_output : Position.square list) : test
    =
  name >:: fun _ ->
  assert_equal expected_output
    (buy_property property person |> get_owned_properties)
    ~printer:(pp_list print_position)
=======

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(* =============== Board tests: ================ *)
(* tests init, position_int, and move_to at once *)
let board_move_to_test (name : string) (board : Board.t) (roll : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.position_int (Board.move_to board roll))
    ~printer:string_of_int
>>>>>>> e1c094dbd2c1ea62abcea831fe28120cb09c7d8e

(* and bank at the same time *)

<<<<<<< HEAD
(* Tests [tile_owned] *)
let tile_owned_test (name : string) (player : Player.t) (tile : Position.square)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (tile_owned player tile) ~printer:string_of_bool

(* test abstract: new_player, move, buy_property. Concrete = get_board?
   get_owned_properties, current_location, tile_owned, *)

(* test abstract: . Concrete = get_board?, tile_owned *)

let move_tests =
=======
let board_tests =
>>>>>>> e1c094dbd2c1ea62abcea831fe28120cb09c7d8e
  [
    new_player_test "starting state" "Alexandra" alexandra0 Raylib.Color.blue;
    (* Don't move *)
    move_test "move 0 from start" alexandra0 0 0;
    move_test "move 0 from 35" juji35 0 35;
    (*move fowards randomly*)
    move_test "move 3" alexandra0 3 3;
    move_test "move 37" alexandra0 37 37;
    move_test "move 15 from 21" brooke21 15 36;
    move_test "move 4 from 37" sophie37 4 1;
    move_test "move 33 from 21" brooke21 33 14;
    (*move fowards, end on start*)
    move_test "move 40" alexandra0 40 0;
    move_test "roll 19 from 21" brooke21 19 0;
    (*move backwards randomly*)
    move_test "move back 3 from start" alexandra0 (-3) 37;
    move_test "move back 16 from start" alexandra0 (-16) 24;
    move_test "move back 15 from 21" brooke21 (-15) 6;
    move_test "move back 6 from 37" sophie37 (-6) 31;
    (*move backwards, pass one round*)
    (*move_test "move back 50 from start" alexandra0 (-50) 30;*)
    (*move_test "move back 81 from start" alexandra0 (-81) 39;*)
    move_test "move back 33 from 21" brooke21 (-33) 28;
    (*move backwards, land on start*)
    move_test "move back 40 from start" alexandra0 (-40) 0;
    (*move_test "move back 80 from start" alexandra0 (-80) 0;*)
    move_test "move back 37 from 37" sophie37 (-37) 0;
  ]

let property_tests =
  [
    get_owned_properties_test "No properties" alexandra0 [];
    get_owned_properties_test "One property" brookeboard [ boardwalk ];
    get_owned_properties_test "A (red) set of properties" jujired
      [ illinois; indiana; kentucky ];
    get_owned_properties_test "Alot of random properties" sophierich
      [ marvin; james; oriental; baltic ];
    buy_property_test "Buy property when having none" alexandra0 ventnor
      [ ventnor ];
    buy_property_test "Buy property when having one different property"
      brookeboard ventnor [ ventnor; boardwalk ];
    buy_property_test "Buy property when already having the same property"
      brookeboard boardwalk [ boardwalk ];
    buy_property_test "Buy property when having many different properties"
      sophierich ventnor
      [ ventnor; marvin; james; oriental; baltic ];
    buy_property_test "Buy property when already owning it" sophierich marvin
      [ marvin; james; oriental; baltic ];
    buy_property_test "Buy property when already owning it" sophierich baltic
      [ marvin; james; oriental; baltic ];
    (* The following should test true*)
    tile_owned_test "True: One correct\n   property owned" brookeboard boardwalk
      true;
    tile_owned_test "True: Property owned among many" sophierich marvin true;
    tile_owned_test "True: Property owned among many" sophierich baltic true;
    (* The following should test false*)
    tile_owned_test "False: No properties owned" alexandra0 ventnor false;
    tile_owned_test "False: Property not owned while owning one property"
      brookeboard ventnor false;
    tile_owned_test "False: Property owned by someone else" sophierich boardwalk
      false;
    tile_owned_test
      "False: Property owned by no one (among many properties owned)" sophierich
      ventnor false;
    tile_owned_test
      "False: Player has different property in the same set (orange)" sophierich
      tennessee false;
  ]

let init_test (name : string) f (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (current f) ~printer:string_of_int

let pay_receive_test (name : string) f (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (current f) ~printer:string_of_int

let broke_test (name : string) (int : int) : test =
  name >:: fun _ -> assert_raises Broke (fun _ -> pay int init)

let account_tests =
  [
    init_test "new account has 1500 dollars" init 1500;
    pay_receive_test "new account pay 500 is 1000" (pay 500 init) 1000;
    pay_receive_test "new account pay 1500 is 0" (pay 1500 init) 0;
    broke_test "new account pay 1501 raises Broke" 1501;
    pay_receive_test "new account receive 100 is 1600" (receive 100 init) 1600;
    pay_receive_test "new account receive 0 is 1500" (receive 0 init) 1500;
  ]

let parse_test (name : string) f (str : string) (expected_output : command) :
    test =
  name >:: fun _ -> assert_equal expected_output (parse str)

let parse_empty_test (name : string) f (str : string) =
  name >:: fun _ -> assert_raises Empty (fun _ -> parse str)

let parse_malformed_test (name : string) f (str : string) =
  name >:: fun _ -> assert_raises Malformed (fun _ -> parse str)

let command_tests =
  [
    parse_test "roll is Roll" parse "roll" Roll;
    parse_test "quit is Quit" parse "quit" Quit;
    parse_test "\"  roll    \" is Roll" parse "  roll    " Roll;
    parse_test "\"     quit\" is Quit" parse "     quit" Quit;
    parse_empty_test "empty string raises Empty" parse "";
    parse_empty_test "spaces string raises Empty" parse "     ";
    parse_malformed_test "Roll raises Malformed" parse "Roll";
    parse_malformed_test "Quit raises Malformed" parse "Quit";
    parse_malformed_test "gibberish raises Malformed" parse "ewojuriegybthvfj";
    parse_test "purchase is Purchase" parse "purchase" Purchase;
    parse_test "\"purchase \" is Purchase" parse "purchase " Purchase;
    parse_test "roll quit is Roll" parse "roll quit" Roll;
    parse_test "quit ewjiohgu is Roll" parse "quit ewjiohgu" Quit;
    parse_malformed_test "rollquit is Malformed" parse "rollquit";
    parse_test "end is Endturn" parse "end" EndTurn;
    parse_test "end with spaces is Endturn" parse "    end     " EndTurn;
    parse_test "end with extra symbols after is EndTurn" parse
      "    end    dnfr " EndTurn;
    parse_malformed_test "end with extra symbols before is Malformed" parse
      "  njwr  end    dnfr ";
  ]

let player_get_name_test (name : string) f (player : Player.t)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (f player)

let account_test (name : string) f (player : Player.t) (expected_output : int) :
    test =
  name >:: fun _ -> assert_equal expected_output (f player) ~printer:pp_int

let get_color_test (name : string) f (player : Player.t)
    (expected_output : Raylib.Color.t) : test =
  name >:: fun _ -> assert_equal expected_output (f player)

let get_owned_properties_test (name : string) f (player : Player.t)
    (expected_output : Position.square list) : test =
  name >:: fun _ ->
  assert_equal expected_output (f player) ~cmp:cmp_set_like_lists

let current_location_test (name : string) f (player : Player.t)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (get_index (f player))

let rails_owned_test (name : string) f (board : Position.t) (player : Player.t)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (f board player)

let utilities_owned_test (name : string) f (board : Position.t)
    (player : Player.t) (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (f board player)

let deposit_test (name : string) f (i : int) (player : Player.t)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (account (f i player))

let withdraw_test (name : string) f (i : int) (player : Player.t)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (account (f i player))

let get_jail_test (name : string) f (player : Player.t) (expected_output : bool)
    : test =
  name >:: fun _ -> assert_equal expected_output (f player)

let go_to_jail_test (name : string) f (player : Player.t)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (get_jail (f player))

let free_from_jail_test (name : string) f (player : Player.t)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (get_jail (f player))

let broke_player_test (name : string) f (i : int) (player : Player.t) : test =
  name >:: fun _ -> assert_raises Broke (fun _ -> f i player)

let mediterranean = get_square_from_name "Mediterranean Avenue"
let vermont = get_square_from_name "Vermont Avenue"
let connecticut = get_square_from_name "Connecticut Avenue"
let charles = get_square_from_name "St. Charles Place"
let states = get_square_from_name "States Avenue"
let virginia = get_square_from_name "Virginia Avenue"
let tennessee = get_square_from_name "Tennessee Avenue"
let ny = get_square_from_name "New York Avenue"
let kentucky = get_square_from_name "Kentucky Avenue"
let indiana = get_square_from_name "Indiana Avenue"
let illinois = get_square_from_name "Illinois Avenue"
let atlantic = get_square_from_name "Atlantic Avenue"
let marvin = get_square_from_name "Marvin Gardens"
let pacific = get_square_from_name "Pacific Avenue"
let nc = get_square_from_name "North Carolina Avenue"
let pennsylvania = get_square_from_name "Pennsylvania Avenue"
let park_place = get_square_from_name "Park Place"
let penn_rail = get_square_from_name "Pennsylvania Railroad"
let short_line = get_square_from_name "Short Line Railroad"
let player1 = new_player "player1" Raylib.Color.red

let player2 =
  player1 |> buy_property baltic |> buy_property oriental
  |> buy_property boardwalk |> move 2 board
  |> buy_property reading_railroad

let player3 = player1 |> move 41 board |> buy_property water_works |> go_to_jail
let player4 = player2 |> buy_property bo_railroad
let player5 = player3 |> buy_property electric_company

let player_tests =
  [
    player_get_name_test "name of player1 is player1" Player.get_name player1
      "player1";
    player_get_name_test "name of player2 is player1" Player.get_name player1
      "player1";
    player_get_name_test "name of player3 is player1" Player.get_name player1
      "player1";
    player_get_name_test "name of player4 is player1" Player.get_name player1
      "player1";
    player_get_name_test "name of player5 is player1" Player.get_name player1
      "player1";
    account_test "account of player1 is 1500" account player1 1500;
    account_test "account of player2 is 740" account player2 740;
    account_test "account of player3 is 1350" account player3 1350;
    account_test "account of player4 is 540" account player4 540;
    account_test "account of player5 is 1200" account player5 1200;
    get_color_test "color of player1 is red" get_color player1 Raylib.Color.red;
    get_color_test "color of player2 is red" get_color player2 Raylib.Color.red;
    get_color_test "color of player3 is red" get_color player3 Raylib.Color.red;
    get_color_test "color of player4 is red" get_color player4 Raylib.Color.red;
    get_color_test "color of player5 is red" get_color player5 Raylib.Color.red;
    get_owned_properties_test "player1 has no properties" get_owned_properties
      player1 [];
    get_owned_properties_test
      "player2 owns baltic, oriental, boardwalk and reading railroad"
      get_owned_properties player2
      [ baltic; oriental; boardwalk; reading_railroad ];
    get_owned_properties_test "player3 owns water works" get_owned_properties
      player3 [ water_works ];
    get_owned_properties_test
      "player4 owns baltic, oriental, boardwalk, reading railroad, and bo \
       railroad"
      get_owned_properties player4
      [ baltic; oriental; boardwalk; reading_railroad; bo_railroad ];
    get_owned_properties_test "player5 owns water works and electric company"
      get_owned_properties player5
      [ water_works; electric_company ];
    current_location_test "player1 location is 0" current_location player1 0;
    current_location_test "player2 location is 2" current_location player2 2;
    current_location_test "player3 location is 1" current_location player3 1;
    current_location_test "player4 location is 2" current_location player4 2;
    current_location_test "player5 location is 1" current_location player5 1;
    current_location_test "sophie37 location is 37" current_location sophie37 37;
    rails_owned_test "player1 owns no rails" rails_owned board player1 0;
    rails_owned_test "player2 owns 1 rail" rails_owned board player2 1;
    rails_owned_test "player3 owns 0 rail" rails_owned board player3 0;
    rails_owned_test "player4 owns 2 rail" rails_owned board player4 2;
    rails_owned_test "player5 owns 0 rail" rails_owned board player5 0;
    utilities_owned_test "player1 owns no utilities" util_owned board player1 0;
    utilities_owned_test "player2 owns 0 utility" util_owned board player2 0;
    utilities_owned_test "player3 owns 1 utility" util_owned board player3 1;
    utilities_owned_test "player4 owns 0 utility" util_owned board player4 0;
    utilities_owned_test "player5 owns 2 utility" util_owned board player5 2;
    deposit_test "deposit 100 into player1 is 1600" deposit 100 player1 1600;
    deposit_test "deposit 240 into player2 is 980" deposit 240 player2 980;
    deposit_test "deposit ~-100 into player3 is 1250" deposit ~-100 player3 1250;
    withdraw_test "withdraw 100 from player1 is 1400" withdraw 100 player1 1400;
    withdraw_test "withdraw 1500 from player1 is 0" withdraw 1500 player1 0;
    withdraw_test "400 from player2 is 340" withdraw 400 player2 340;
    broke_player_test "withdraw 1501 from player1 raises Broke" withdraw 1501
      player1;
    broke_player_test "withdraw 1400 from player3 raises Broke" withdraw 1400
      player3;
    get_jail_test "player1 is not in jail" get_jail player1 false;
    get_jail_test "player3 is in jail" get_jail player3 true;
    get_jail_test "player2 is not in jail" get_jail player2 false;
    get_jail_test "player4 is not in jail" get_jail player4 false;
    get_jail_test "player5 is in jail" get_jail player5 true;
    go_to_jail_test "player1 is in jail if they go to jail" go_to_jail player1
      true;
    go_to_jail_test "player2 is in jail if they go to jail" go_to_jail player2
      true;
    go_to_jail_test "player4 is in jail if they go to jail" go_to_jail player4
      true;
    free_from_jail_test
      "player1 is not in jail if they go to jail then are free from jail"
      free_from_jail (go_to_jail player1) false;
    free_from_jail_test
      "player2 is not in jail if they go to jail then are free from jail"
      free_from_jail (go_to_jail player2) false;
    free_from_jail_test
      "player3 is not in jail if they go to jail then are free from jail"
      free_from_jail (go_to_jail player3) false;
    free_from_jail_test
      "player4 is not in jail if they go to jail then are free from jail"
      free_from_jail (go_to_jail player4) false;
    free_from_jail_test
      "player5 is not in jail if they go to jail then are free from jail"
      free_from_jail (go_to_jail player5) false;
  ]

let position_get_name_test (name : string) f (t : square)
    (expected_output : string) =
  name >:: fun _ -> assert_equal expected_output (f t)

let get_index_test (name : string) f (t : square) (expected_output : int) =
  name >:: fun _ -> assert_equal expected_output (f t)

let get_initial_test (name : string) f (b : Position.t)
    (expected_output : square) =
  name >:: fun _ -> assert_equal expected_output (f b)

let start = get_square_from_name "Go"

let square_index_test (name : string) f (b : Position.t) (i : int)
    (expected_output : square) =
  name >:: fun _ -> assert_equal expected_output (f b i)

let square_name_test (name : string) f (b : Position.t) (s : string)
    (expected_output : square) =
  name >:: fun _ -> assert_equal expected_output (f b s)

let position_tests =
  [
    position_get_name_test "name of baltic is Baltic Avenue" Position.get_name
      baltic "Baltic Avenue";
    position_get_name_test "name of reading_railroad is Reading Railroad"
      Position.get_name reading_railroad "Reading Railroad";
    position_get_name_test "name of water_works is Water Works"
      Position.get_name water_works "Water Works";
    position_get_name_test "name of bo_railroad is B&O Railroad"
      Position.get_name bo_railroad "B&O Railroad";
    position_get_name_test "name of electric_company is Electric Company"
      Position.get_name electric_company "Electric Company";
    get_index_test "index of baltic is 3" get_index baltic 3;
    get_index_test "index of reading_railroad is 5" get_index reading_railroad 5;
    get_index_test "index of water_works is 28" get_index water_works 28;
    get_index_test "index of bo_railroad is 25" get_index bo_railroad 25;
    get_index_test "index of electric_company is 12" get_index electric_company
      12;
    get_index_test "index of start is 0" get_index start 0;
    get_initial_test "initial square is Go" get_initial board start;
    square_index_test "square at index 3 is baltic" square_index board 3 baltic;
    square_index_test "square at index 5 is reading_railroad" square_index board
      5 reading_railroad;
    square_index_test "square at index 28 is water_works" square_index board 28
      water_works;
    square_index_test "square at index 25 is bo_railroad" square_index board 25
      bo_railroad;
    square_index_test "square at index 12 is electric_company" square_index
      board 12 electric_company;
    square_index_test "square at index 0 is start" square_index board 0 start;
    square_name_test "square with name Baltic Avenue is baltic" square_name
      board "Baltic Avenue" baltic;
    square_name_test "square with name Reading Railraod is reading_railroad"
      square_name board "Reading Railroad" reading_railroad;
    square_name_test "square with name Water Works is water_works" square_name
      board "Water Works" water_works;
    square_name_test "square with name Go is start" square_name board "Go" start;
    square_name_test "square with name B&O Railroad is bo_railroad" square_name
      board "B&O Railroad" bo_railroad;
    square_name_test "square with name Electric Company is electric_company"
      square_name board "Electric Company" electric_company;
  ]

(* =============== Player tests: ================ *)

(* make a few players at various locations, with various properties: *)
let alexandra0 = new_player "Alexandra"
let brooke21 = move 21 (new_player "Brooke")
let juji35 = move 35 (new_player "Juji")
let sophie37 = move 37 (new_player "Sophie")

(* Tests that [new_player] is initialized to the correct values using
   [current_location], [get_owned_properties], and [get_board]*)
let new_player_test (name : string) (player : Player.t) : test =
  name >:: fun _ ->
  assert_equal 0 (current_location player);
  assert_equal [] (get_owned_properties player);
  assert_equal Board.init (get_board player)

(* Tests [move] after the player has moved [x] spots, using [current_location]*)
let move_test (name : string) (person : Player.t) (x : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (move x person |> current_location)
    ~printer:string_of_int

(* Give the players properties *)
let brookeboard = buy_property "Boardwalk" brooke21

let jujired =
  buy_property "Kentucky Avenue" juji35
  |> buy_property "Indiana Avenue"
  |> buy_property "Illinois Avenue"

let sophierich =
  buy_property "Baltic Avenue" sophie37
  |> buy_property "Oriental Avenue"
  |> buy_property "St. James Place"
  |> buy_property "Marvin Gardens"

(* Tests [get_owned_properites] *)
let get_owned_properties_test (name : string) (person : Player.t)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_owned_properties person)
    ~printer:(pp_list pp_string)

(* Tests [buy_property] using [get_owned_properties] *)
let buy_property_test (name : string) (person : Player.t) (property : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (buy_property property person |> get_owned_properties)
    ~printer:(pp_list pp_string)
(* and bank at the same time *)

(* Tests [tile_owned] *)
let tile_owned_test (name : string) (player : Player.t) (tile : string)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (tile_owned player tile) ~printer:string_of_bool

(* test abstract: new_player, move, buy_property. Concrete = get_board?
   get_owned_properties, current_location, tile_owned, *)

(* test abstract: . Concrete = get_board?, tile_owned *)

let move_tests =
  [
    new_player_test "starting state" alexandra0;
    (* Don't move *)
    move_test "move 0 from start" alexandra0 0 0;
    move_test "move 0 from 35" juji35 0 35;
    (*move fowards randomly*)
    move_test "move 3" alexandra0 3 3;
    move_test "move 37" alexandra0 37 37;
    move_test "move 15 from 21" brooke21 15 36;
    (*move fowards, pass one round*)
    move_test "move 50 from start" alexandra0 50 10;
    move_test "move 81 from start" alexandra0 81 1;
    move_test "move 4 from 37" sophie37 4 1;
    move_test "move 33 from 21" brooke21 33 54;
    (*move fowards, end on start*)
    move_test "move 40" alexandra0 40 0;
    move_test "roll 19 from 21" brooke21 19 0;
    (*move backwards randomly*)
    move_test "move back 3 from start" alexandra0 (-3) 37;
    move_test "move back 16 from start" alexandra0 (-16) 24;
    move_test "move back 15 from 21" brooke21 (-15) 6;
    move_test "move back 6 from 37" sophie37 (-6) 31;
    (*move backwards, pass one round*)
    move_test "move back 50 from start" alexandra0 (-50) 30;
    move_test "move back 81 from start" alexandra0 (-81) 39;
    move_test "move back 33 from 21" brooke21 (-33) 28;
    (*move backwards, land on start*)
    move_test "move back 40 from start" alexandra0 (-40) 0;
    move_test "move back 80 from start" alexandra0 (-80) 0;
    move_test "move back 37 from 37" sophie37 (-37) 12;
  ]

let property_tests =
  [
    get_owned_properties_test "No properties" alexandra0 [];
    get_owned_properties_test "One property" brookeboard [ "Boardwalk" ];
    get_owned_properties_test "A (red) set of properties" jujired
      [ "Illinois Avenue"; "Indiana Avenue"; "Kentucky Avenue" ];
    get_owned_properties_test "A lot of random properties" sophierich
      [
        "Marvin Gardens"; "St. James Place"; "Oriental Avenue"; "Baltic Avenue";
      ];
    buy_property_test "Buy property when having none" alexandra0 "Ventor Avenue"
      [ "Ventor Avenue" ];
    buy_property_test "Buy property when having one different property"
      brookeboard "Ventor Avenue"
      [ "Ventor Avenue"; "Boardwalk" ];
    buy_property_test "Buy property when already having the same property"
      brookeboard "Boardwalk" [ "Boardwalk" ];
    buy_property_test "Buy property when having many different properties"
      sophierich "Ventor Avenue"
      [
        "Ventor Avenue";
        "Marvin Gardens";
        "St. James Place";
        "Oriental Avenue";
        "Baltic Avenue";
      ];
    buy_property_test "Buy property when already owning it" sophierich
      "Marvin Gardens"
      [
        "Marvin Gardens"; "St. James Place"; "Oriental Avenue"; "Baltic Avenue";
      ];
    buy_property_test "Buy property when already owning it" sophierich
      "Baltic Avenue"
      [
        "Marvin Gardens"; "St. James Place"; "Oriental Avenue"; "Baltic Avenue";
      ];
    (* The following should return an error or a nop, please clarify the
       spec... *)
    buy_property_test
      "Buy property that someone else already own (IM ASSUMING THIS IS ILLEGAL)"
      jujired "Boardwalk"
      [ "Illinois Avenue"; "Indiana Avenue"; "Kentucky Avenue" ];
    buy_property_test "Buy invalid property: non existent" jujired ""
      [ "Illinois Avenue"; "Indiana Avenue"; "Kentucky Avenue" ];
    buy_property_test "Buy invalid property: misspelled" jujired
      "Not a property"
      [ "Illinois Avenue"; "Indiana Avenue"; "Kentucky Avenue" ];
    buy_property_test "Buy invalid property: Caps" jujired "ventor avenue"
      [ "Illinois Avenue"; "Indiana Avenue"; "Kentucky Avenue" ];
    buy_property_test "Buy invalid property: white spaces" jujired
      "    Ventor   Avenue   "
      [ "Illinois Avenue"; "Indiana Avenue"; "Kentucky Avenue" ];
    (* The following should test true*)
    tile_owned_test "True: One correct property owned" brookeboard "Boardwalk"
      true;
    tile_owned_test "True: Property owned among many" sophierich
      "Marvin Gardens" true;
    tile_owned_test "True: Property owned among many" sophierich "Baltic Avenue"
      true;
    (* The following should test false*)
    tile_owned_test "False: No properties owned" alexandra0 "Ventor Avenue"
      false;
    tile_owned_test "False: Property not owned while owning one property"
      brookeboard "Ventor Avenue" false;
    tile_owned_test "False: Property owned by someone else" sophierich
      "Boardwalk" false;
    tile_owned_test
      "False: Property owned by no one (among many properties owned)" sophierich
      "Ventor Avenue" false;
    tile_owned_test
      "False: Player has different property in the same set (orange)" sophierich
      "Tennessee Avenue" false;
    (* The following should give an error or false*)
    tile_owned_test "Invalid property: non existent" jujired "" false;
    tile_owned_test "Invalid property: misspelled" jujired "Not a property"
      false;
    tile_owned_test "Invalid property: Caps" jujired "illinois avenue" false;
    tile_owned_test "Invalid property: white spaces" jujired
      "    Kentucky   Avenue   " false;
  ]

let player_tests = move_tests @ property_tests

let suite =
  "test suite for Monopoly"
<<<<<<< HEAD
  >::: List.flatten
         [
           move_tests;
           property_tests;
           account_tests;
           command_tests;
           player_tests;
           position_tests;
         ]
=======
  >::: List.flatten [ player_tests; board_tests; account_tests ]
>>>>>>> e1c094dbd2c1ea62abcea831fe28120cb09c7d8e

let _ = run_test_tt_main suite
