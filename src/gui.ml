open Raylib
open Player

(*window size*)

(*window width and height; width > height*)
let width = 1000
let height = 800
let text_color = Color.black

(*board*)

(* Resetable metrics: *)
let board_background_color = Color.green
let board_line_color = Color.black

(* height of the board; must be less than 1. The board is a square *)
let b_height = height * 3 / 4
let prop_from_top = 0.8 (* must be less than 1 *)
let start_prop = b_height / 6 (* size of the corner squares *)

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

let grid_height = start_prop
let grid_width = (b_height - (2 * start_prop)) / 9

(* Player: *)
let p1col = Color.red
let p2col = Color.blue
let p_size = 0.2
let (p : Player.t) = new_player "Juji"

(* [center_text string] prints [string] in the middle of the page*)
let center_text string =
  let font_size =
    int_of_float
      (float_of_int width /. (float_of_int (String.length string) *. 0.5) *. 0.3)
  in
  draw_text string
    ((width / 2)
    - (int_of_float (float_of_int (String.length string) *. 0.3) * font_size))
    ((height - font_size) / 2)
    font_size text_color

(* [print_stats player] prints the name, balance, position, and properties owned
   of the current player *)
let print_player_stats (player : Player.t) =
  let font_size = 5 in
  let spacing = 3 in
  let xindent = width * 5 / 6 in
  let yindent = font_size + spacing in
  draw_text
    ("Player Name: " ^ Player.get_name player)
    xindent yindent font_size text_color;
  draw_text "Current Balance: " xindent (yindent * 2) font_size text_color;
  draw_text
    ("Current Location: " ^ string_of_int (Position.get_index (Player.current_location player)))
    xindent (yindent * 3) font_size text_color;
  let properties = Player.get_owned_properties player in
  draw_text "Properties Owned:" xindent (yindent * 4) font_size text_color;
  for x = 0 to List.length properties - 1 do
    draw_text (Position.get_name (List.nth properties x)) xindent
      ((yindent * 5) + x)
      font_size text_color
  done

let setup () =
  Raylib.init_window width height "MONOPOLY!!!";
  Raylib.set_target_fps 60

(* [draw_player position color] draws a circle representing a player in the
   correct [position] with the color [color]. Requires [position] is between 0
   and 39, inclusive *)
let draw_player position color =
  let p_radius = float_of_int grid_width *. p_size in
  let p_leftx = fst board_tl + (start_prop / 2) in
  let p_rightx =
    (fst board_tl + start_prop + (grid_width * 9) + fst board_tr) / 2
  in
  let p_topy = snd board_tl + (start_prop / 2) in
  let p_bottomy =
    (snd board_tl + start_prop + (9 * grid_width) + snd board_bl) / 2
  in
  if position > 30 then
    (* positions 39 - 31: *)
    draw_circle p_rightx
      (snd board_tr + start_prop
      + (grid_width * (position - 30))
      - (grid_width / 2))
      p_radius color
  else if position > 20 then
    (* player at top edge, positions 30 - 21 *)
    let p_y = snd board_tl + (start_prop / 2) in
    if position == 30 then
      (* position 30: top right corner *)
      draw_circle p_rightx p_y p_radius color
    else
      (* positions 29 - 21: *)
      draw_circle
        (fst board_bl + start_prop
        + (grid_width * (position - 20))
        - (grid_width / 2))
        p_y p_radius color
  else if position > 10 then
    (* player at left edge; positions 20 - 11 *)
    if position == 20 then
      (* position 20: top left corner *)
      draw_circle p_leftx p_topy p_radius color
    else
      (* positions 19 - 11: *)
      draw_circle p_leftx
        (snd board_tr + start_prop
        + (grid_width * (20 - position))
        - (grid_width / 2))
        p_radius color
  else if position == 10 then
    (* position 10: bottom left corner *)
    draw_circle p_leftx p_bottomy p_radius color
  else if position > 0 then
    (* positions 9 - 1: *)
    draw_circle
      (fst board_bl + start_prop
      + (grid_width * (10 - position))
      - (grid_width / 2))
      p_bottomy p_radius color
  else
    (* position 0: bottom right corner (start) *)
    draw_circle p_rightx p_bottomy p_radius color

(** [draw_board_base] draws the basic outline of the game board *)
let draw_board_base () =
  draw_rectangle (fst board_tl) (snd board_tl) b_height b_height
    board_background_color;
  (* Draw board outline: *)
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
  (* Draw board gridlines: *)
  (* draw bottom gridlines *)
  for x = 0 to 9 do
    draw_line
      (fst board_bl + start_prop + (grid_width * x))
      (snd board_bl)
      (fst board_bl + start_prop + (grid_width * x))
      (snd board_tr + start_prop + (grid_width * 9))
      board_line_color
  done;
  (* draw top gridlines *)
  for x = 0 to 9 do
    draw_line
      (fst board_bl + start_prop + (grid_width * x))
      (snd board_tl)
      (fst board_bl + start_prop + (grid_width * x))
      (snd board_tl + grid_height)
      board_line_color
  done;
  (* draw left gridlines *)
  for x = 0 to 9 do
    draw_line (fst board_tl)
      (snd board_tl + start_prop + (grid_width * x))
      (fst board_tl + start_prop)
      (snd board_tl + start_prop + (grid_width * x))
      board_line_color
  done;
  (* draw right gridlines *)
  for x = 0 to 9 do
    draw_line (fst board_tr)
      (snd board_tr + start_prop + (grid_width * x))
      (fst board_tl + start_prop + (grid_width * 9))
      (snd board_tr + start_prop + (grid_width * x))
      board_line_color
  done;
  (* draw inner square *)
  (* inner top *)
  draw_line (fst board_tl)
    (snd board_tl + start_prop)
    (fst board_tr)
    (snd board_tr + start_prop)
    board_line_color;
  (* inner bottom *)
  draw_line (fst board_tl)
    (snd board_tl + start_prop + (9 * grid_width))
    (fst board_tr)
    (snd board_tr + start_prop + (9 * grid_width))
    board_line_color;
  (* inner left *)
  draw_line
    (fst board_tl + start_prop)
    (snd board_tl)
    (fst board_bl + start_prop)
    (snd board_bl) board_line_color;
  (* inner right *)
  draw_line
    (fst board_tl + start_prop + (grid_width * 9))
    (snd board_tl)
    (fst board_tl + start_prop + (grid_width * 9))
    (snd board_br) board_line_color

let rec draw_board () =
  begin_drawing ();
  clear_background Color.white;
  (* Draw board base: *)
  draw_board_base ();
  (* Add text: *)
  center_text "Let's Play!";
  draw_text "Press B for back"
    (width - (width / 7))
    (height * 24 / 25) (height / 30) Color.red;
  (* Add the player: *)
  draw_player (Position.get_index (Player.current_location p)) p1col;
  print_player_stats p;
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
    center_text "Welcome to Monopoly! 2 Player Edition Press Space to continue";
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
