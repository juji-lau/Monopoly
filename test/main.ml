open OUnit2
open Monopoly
open Player
open Position
open Account
open Command

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

(* =============== Player tests: ================ *)
(* make a board *)
let board = Position.new_board

let print_position position =
  "square name: " ^ get_name position ^ " at position: "
  ^ string_of_int (get_index position)

(* helper function to get the square from the name *)
let get_square_name pos = Position.square_name board pos

(* grab a few squares: *)
let baltic = get_square_name "Baltic Avenue"
let oriental = get_square_name "Oriental Avenue"
let james = get_square_name "St. James Place"
let tennessee = get_square_name "Tennessee Avenue"
let ventnor = get_square_name "Ventnor Avenue"
let kentucky = get_square_name "Kentucky Avenue"
let indiana = get_square_name "Indiana Avenue"
let illinois = get_square_name "Illinois Avenue"
let marvin = get_square_name "Marvin Gardens"
let boardwalk = get_square_name "Boardwalk"

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

(* and bank at the same time *)

(* Tests [tile_owned] *)
let tile_owned_test (name : string) (player : Player.t) (tile : Position.square)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (tile_owned player tile) ~printer:string_of_bool

(* test abstract: new_player, move, buy_property. Concrete = get_board?
   get_owned_properties, current_location, tile_owned, *)

(* test abstract: . Concrete = get_board?, tile_owned *)

let move_tests =
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
  ]

let suite =
  "test suite for Monopoly"
  >::: List.flatten [ move_tests; property_tests; account_tests; command_tests ]

let _ = run_test_tt_main suite
