open OUnit2
open Monopoly
open Board
open Player
open Position
open Account

let current_location_test (name : string) (input : Player.t)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (input |> current_location)
    ~printer:string_of_int

(*tests init, position_int, and move_to at once *)
let board_move_to_test (name : string) (board : Board.t) (roll : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.position_int (Board.move_to board roll)) ~printer: string_of_int 

let player_tests = [ current_location_test "start" (new_player "Alexandra") 0 ]

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

let account_init_test (name : string) (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (current init)

let account_pay_test (name : string) (i : int) (a : Account.t)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output a

let account_tests =
  [ account_init_test "initial amount is 1500" 1500; account_pay_test ]

let suite =
  "test suite for Monopoly" >::: List.flatten [ player_tests; account_tests ]

let _ = run_test_tt_main suite
