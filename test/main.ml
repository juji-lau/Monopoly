open OUnit2
open Board
open Player

let current_location_test (name : string) (input : string) (expected_output:int) : test = 
  name >:: fun _ -> assert_equal expected_output (input |> Player.new_player |> Player.current_location)
  
  let current_location_tests =
    [
      current_room_id_test "ho plaza's current room at start is ho plaza"
        (init_state (from_json ho))
        "ho plaza";
    ]
  let suite =
    "test suite for A2"
    >::: List.flatten [ cmp_demo; adventure_tests; command_tests; state_tests ]
  
  let _ = run_test_tt_main suite
  