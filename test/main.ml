
let current_location_test (name : string) (input : string) (expected_output:int) : test = 
  name >:: fun _ -> assert_equal expected_output (input |> Player.new_player |> Player.current_location)