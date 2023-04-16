open Raylib

(*window size*)

(*window width and height; width > height*)
let width = 1000
let height = 500
let text_color = Color.black

(*board*)
let board_background_color = Color.darkgreen
let board_line_color = Color.black

(* height of the board; must be less than 1 *)
let b_height = height * 3/4

(* must be less than 1 *)
let prop_from_top = 0.8
(*let board_length = width * 3 / 4*)

(* (0,0) is the top left of the window; (0, height) is the bottom left of the
   window *)
let board_tl =
  ( (width - b_height) / 2,
    int_of_float (float_of_int (height - b_height) *. prop_from_top) )

let board_tr =
  ( (width / 2) + (b_height / 2),
    int_of_float (float_of_int (height - b_height) *. prop_from_top) )

let board_bl =
  ( (width - b_height) / 2,
    int_of_float (float_of_int (height - b_height) *. prop_from_top) + b_height
  )

let board_br =
  ( (width / 2) + (b_height / 2),
    int_of_float (float_of_int (height - b_height) *. prop_from_top) + b_height
  )

let center_text string =
  let font_size = width / String.length string * 3 / 4 in
  draw_text string
    ((width - (String.length string * font_size)) / 2)
    ((height + font_size) / 2)
    font_size text_color

let setup () =
  Raylib.init_window width height "raylib [core] example - basic window";
  Raylib.set_target_fps 60

(* draw the second page*)
let rec check_start () =
  begin_drawing ();
  clear_background Color.green;
  (*draw_text "Let's Play!" (width / 4) (height / 2) (height / 15)
    Color.black;*)
  center_text "Let's Play!";
  draw_text "Press B for back"
    (width - (width / 7))
    (height / 25) (height / 30) Color.red;
  end_drawing ();
  (* if B is pressed, keep drawing this page, otherwise, leave function to the
     first page *)
  if is_key_down Key.B == false then check_start ()

(*tentatively draw the base game board*)
let rec draw_board () =
  begin_drawing ();
  clear_background Color.white;
  draw_rectangle (fst board_tl)
    (snd board_tl)
    (b_height) (b_height) board_background_color;
  (*outline board*)
  (*top line*)
  draw_line (fst board_tl) (snd board_tl) (fst board_tr) (snd board_tr)
    board_line_color;
  (*bottom line*)
  draw_line (fst board_bl) (snd board_bl) (fst board_br) (snd board_br)
    board_line_color;
  (*left line*)
  draw_line (fst board_tl) (snd board_tl) (fst board_bl) (snd board_bl)
    board_line_color;
  (*right line*)
  draw_line (fst board_tr) (snd board_tr) (fst board_br) (snd board_br)
    board_line_color;
  end_drawing ();
  if is_key_down Key.B == false then draw_board ()

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
    (*draw_text "Welcome to Monopoly! Press the Space bar to continue" (width /
      4) (height / 2) (height / 15) Color.green;*)
    (* raylib function [is_key_down Key.key] is a bool tells us whether [key] is
       pressed or not*)
    (*if is_key_down Key.Space then check_start ();*)
    if is_key_down Key.Space then draw_board ();
    (* call at the end of each drawing *)
    end_drawing ();
    loop ()

(*let () = setup |> loop*)
