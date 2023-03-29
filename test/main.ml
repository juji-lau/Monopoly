open OUnit2
open Monopoly
open Board
open Player
open Position

(* =============== Board tests: ================ *)
(* tests init, position_int, and move_to at once *)
let board_move_to_test (name : string) (board : Board.t) (roll : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.position_int (Board.move_to board roll))
    ~printer:string_of_int

(* make a few boards to test with:*)
let board0 = Board.init
let board1 = Board.move_to Board.init 1
let board12 = Board.move_to Board.init 12
let board21 = Board.move_to Board.init 21
let board35 = Board.move_to Board.init 35
let board37 = Board.move_to Board.init 37

let board_tests =
  [
    (* don't move*)
    board_move_to_test "roll 0" Board.init 0 0;
    board_move_to_test "roll 0 from 0" board0 0 0;
    board_move_to_test "roll 0 from 35" board35 0 35;
    (*move fowards randomly*)
    board_move_to_test "roll 3" Board.init 3 3;
    board_move_to_test "roll 37" Board.init 37 37;
    board_move_to_test "roll 16 from 0" board0 16 16;
    board_move_to_test "roll 15 from 21" board21 15 36;
    (*move fowards pass one round*)
    board_move_to_test "roll 50" Board.init 50 10;
    board_move_to_test "roll 81" Board.init 81 1;
    board_move_to_test "roll 4 from 37" board37 4 1;
    board_move_to_test "roll 33 from 21" board21 33 54;
    (*move fowards land on start*)
    board_move_to_test "roll 40" Board.init 40 0;
    board_move_to_test "roll 19 from 21" board21 19 0;
    (*move backwards randomly*)
    board_move_to_test "roll -3" Board.init (-3) 37;
    board_move_to_test "roll -16 from 0" board0 (-16) 24;
    board_move_to_test "roll -15 from 21" board21 (-15) 6;
    board_move_to_test "roll -6 from 37" board37 (-6) 31;
    (*move backwards pass one round*)
    board_move_to_test "roll -50" Board.init (-50) 30;
    board_move_to_test "roll -81" Board.init (-81) 39;
    board_move_to_test "roll -14 from 12" board12 (-14) 38;
    board_move_to_test "roll -33 from 21" board21 (-33) 28;
    (*move backwards land on 0*)
    board_move_to_test "roll -40" Board.init (-40) 0;
    board_move_to_test "roll -80 from 0" board0 (-80) 0;
    board_move_to_test "roll -12 from 12" board12 (-12) 12;
  ]

(* =============== Player tests: ================ *)

(* make a few players at various locations: *)
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

(* Test buy_property, get_owned_properties, and bank at the same time *)

(* Test tile_owned *)

(* test abstract: new_player, move, buy_property. Concrete = get_board?
   get_owned_properties, current_location, tile_owned, *)

(* test abstract: , . Concrete = get_board? , tile_owned, *)

let player_tests =
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

let suite =
  "test suite for Monopoly" >::: List.flatten [ player_tests; board_tests ]

let _ = run_test_tt_main suite
