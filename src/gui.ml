open Raylib

let width = 1000
let height = 500
let text_color = Color.black

let center_text string =
  let font_size = width / String.length string * 3 / 4 in
  draw_text string ((width - ((String.length string)* font_size))/2) ((height + font_size)/2) font_size text_color

let setup () =
  Raylib.init_window width height "raylib [core] example - basic window";
  Raylib.set_target_fps 60

(* draw the second page*)
let rec check_start () =
  begin_drawing ();
  clear_background Color.green;
  (*draw_text "Let's Play!" (width / 4) (height / 2) (height / 15) Color.black;*)
  center_text "Let's Play!";
  draw_text "Press B for back"
    (width - (width / 7))
    (height / 25) (height / 30) Color.red;
  end_drawing ();
  (* if B is pressed, keep drawing this page, otherwise, leave function to the first page *)
  if is_key_down Key.B == false then check_start ()

let rec draw_board() = begin_drawing(); draw_grid

(* initial window, all windows must be recursive*)
let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    (* call begin and and drawing between each drawing *)
    begin_drawing ();
    clear_background Color.white;
    (* custom function, prints message on the screen *)
    center_text "Welcome to Monopoly! Press the Space bar to continue...";
    (*draw_text "Welcome to Monopoly! Press the Space bar to continue" (width / 4)
      (height / 2) (height / 15) Color.green;*)
    (* raylib function [is_key_down Key.key] is a bool tells us whether [key] is pressed or not*)
    if is_key_down Key.Space then check_start ();
    (* call at the end of each drawing *)
    end_drawing ();
    loop ()

(*let () = setup |> loop*)
