open OUnit2
open Monopoly
open Board
open Player
open Position

let current_location_test (name : string) (input : Player.t)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (input |> current_location) ~printer: string_of_int
let board_position_test (name : string) (board : Board.t) (roll : int) (expected_output : int) : test =
  name >:: fun _ -> 
    assert_equal expected_output (position (Board.move_to board roll))
let player_tests = [ 
  current_location_test "start" (new_player "Alexandra") 0;
]
let board_tests = [
  board_position_test "roll 0" Board.init 0 0;
  board_position_test "roll 3" Board.init 3 3;
  board_position_test "roll 12" Board.init 12 12;
  board_position_test "roll 37" Board.init 1 1;
]
let suite = "test suite for Monopoly" >::: List.flatten [ player_tests; board_tests ]
let _ = run_test_tt_main suite
