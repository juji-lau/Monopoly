open OUnit2
open Monopoly
open Board
open Player
open Position

let current_location_test (name : string) (input : string)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (input |> new_player |> current_location)

let player_tests = [ current_location_test "start" "Alexandra" 0 ]
let suite = "test suite for Monopoly" >::: List.flatten [ player_tests ]
let _ = run_test_tt_main suite
