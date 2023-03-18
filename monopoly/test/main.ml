
let setup () =
  Raylib.init_window 800 450 "Monopoly";
  Raylib.set_target_fps 60

let draw_rand() = let open Raylib in
  begin_drawing ();
  clear_background Color.raywhite;
  draw_text "move the ball with arrow keys" 10 10 20 Color.darkgray;
  end_drawing ()
let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    clear_background Color.raywhite;
    draw_text "Press the down key to continue" 190 200 20
      Color.lightgray;
    if is_key_down Key.Down then draw_rand();
    loop ()


(*let rec next_window () = 
  if Raylib.window_should_close () then Raylib.close_window () 
  else
    let open Raylib in
    begin_drawing ();
    clear_background Color.raywhite;
    draw_text "Lets play" 190 200 20
      Color.lightgray;
  end_drawing ();
  next_window ()*)

let () = setup () |> loop