open Raylib
open Player

(*window size*)

(*window width and height; width > height*)
let width = 1000
let height = 800

(* custom colors: *)
let text_color = Color.create 0 0 0 255
let fine_text_color = Color.create 50 50 50 100
let board_color = Color.create 2 200 70 100
let board_line_color = Color.black

(*board*)
(* height of the board; must be less than 1. The board is a square *)
let b_height = height * 3 / 4
let prop_from_top = 0.6 (* must be less than 1 *)
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
let (p : Player.t) = new_player "Juji" Raylib.Color.purple

let setup () =
  Raylib.init_window width height "MONOPOLY!!!";
  Raylib.set_target_fps 60

(** [print_insns insns color] prints the instructions [insns] at the bottom of
    the page, under the board. *)
let print_insns insns color =
  let font_size = (height / 55) + 1 in
  let x_indent = fst board_bl in
  let y_indent = (snd board_bl + height) / 2 in
  draw_text "Instructions:  " x_indent y_indent font_size text_color;
  draw_text insns (x_indent + (start_prop * 10 / 9)) y_indent font_size color

(** [center_text line_num total_lines string color] prints [string] in the
    middle of the page with the color [color]. To print longer messages, call
    [center_text] should be called for each line. [line_num] is the line number
    of the message and [total_lines] is the total number of lines in the
    message. *)
let center_text line_num total_lines string color =
  let font_size =
    int_of_float
      (float_of_int width /. (float_of_int (String.length string) *. 0.5) *. 0.3)
  in
  let spacing = font_size in
  let new_total = (total_lines + 1) / 2 * 2 in
  let yindent =
    (float_of_int height /. 2.)
    -. (float_of_int (font_size + spacing) /. 2. *. float_of_int new_total)
  in
  let lineindent =
    yindent +. (float_of_int line_num *. float_of_int (font_size + spacing))
  in
  draw_text string
    ((width / 2)
    - (int_of_float (float_of_int (String.length string) *. 0.3) * font_size))
    (int_of_float lineindent) font_size color

(** [draw_board_base] draws the basic outline of the game board, and the back
    button *)
let draw_board_base () =
  let font_size = 15 in
  let spacing = 10 in
  let xindent = width * 6 / 7 in
  let yindent = height - font_size - spacing in
  draw_text "Press B for back" xindent yindent font_size Raylib.Color.red;
  let num_font_size = 5 in
  let num_indent = start_prop * 5 / 6 in
  draw_rectangle (fst board_tl) (snd board_tl) b_height b_height board_color;
  center_text 1 2 "Monopoly!!!" Raylib.Color.red;
  center_text 2 2 "2 Player Edition" Raylib.Color.red;
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
    let start_line =
      (fst board_bl + start_prop + (grid_width * x), snd board_bl)
    in
    let end_line =
      ( fst board_bl + start_prop + (grid_width * x),
        snd board_tr + start_prop + (grid_width * 9) )
    in
    draw_line (fst start_line) (snd start_line) (fst end_line) (snd end_line)
      board_line_color;
    (* Add bottom numbers 9 - 1 inclusive *)
    if x != 9 then
      draw_text
        (string_of_int (9 - x))
        (fst start_line + (grid_width / 2))
        (snd start_line - start_prop + num_indent)
        num_font_size text_color
  done;
  (* draw top gridlines *)
  for x = 0 to 9 do
    let start_line =
      (fst board_bl + start_prop + (grid_width * x), snd board_tl)
    in
    let end_line =
      (fst board_bl + start_prop + (grid_width * x), snd board_tl + grid_height)
    in
    draw_line (fst start_line) (snd start_line) (fst end_line) (snd end_line)
      board_line_color;
    (* Add top numbers 21 - 29 inclusive *)
    if x != 9 then
      draw_text
        (string_of_int (21 + x))
        (fst start_line + (grid_width / 2))
        (snd start_line + start_prop - num_indent)
        num_font_size text_color
  done;
  (* draw left gridlines *)
  for x = 0 to 9 do
    let start_line =
      (fst board_tl, snd board_tl + start_prop + (grid_width * x))
    in
    let end_line =
      (fst board_tl + start_prop, snd board_tl + start_prop + (grid_width * x))
    in
    draw_line (fst start_line) (snd start_line) (fst end_line) (snd end_line)
      board_line_color;
    (* Add left numbers 11 - 19 inclusive *)
    if x != 9 then
      draw_text
        (string_of_int (19 - x))
        (fst start_line + start_prop - num_indent)
        (snd start_line + (grid_width / 2))
        num_font_size text_color
  done;
  (* draw right gridlines *)
  for x = 0 to 9 do
    let start_line =
      (fst board_tr, snd board_tr + start_prop + (grid_width * x))
    in
    let end_line =
      ( fst board_tl + start_prop + (grid_width * 9),
        snd board_tr + start_prop + (grid_width * x) )
    in
    draw_line (fst start_line) (snd start_line) (fst end_line) (snd end_line)
      board_line_color;
    (* Add right numbers 31 - 39 inclusive *)
    if x != 9 then
      draw_text
        (string_of_int (31 + x))
        (fst end_line + num_indent)
        (snd end_line + (grid_width / 2))
        num_font_size text_color
  done;
  (* Number the corner squares 0, 10, 20 , 30 *)
  draw_text "0"
    (fst board_br - start_prop + num_indent)
    (snd board_br - start_prop + num_indent)
    num_font_size text_color;
  draw_text "10"
    (fst board_bl + start_prop - num_indent)
    (snd board_bl - start_prop + num_indent)
    num_font_size text_color;
  draw_text "20"
    (fst board_tl + start_prop - num_indent)
    (snd board_tl + start_prop - num_indent)
    num_font_size text_color;
  draw_text "30"
    (fst board_tr - start_prop + num_indent)
    (snd board_tr + start_prop - num_indent)
    num_font_size text_color;
  draw_text "START"
    (fst board_br
    - ((start_prop / 2)
      + int_of_float
          (float_of_int font_size *. 0.4 *. float_of_int (String.length "start"))
      ))
    (snd board_br
    - ((start_prop / 2)
      + int_of_float
          (float_of_int font_size *. 0.1 *. float_of_int (String.length "start"))
      ))
    font_size Raylib.Color.red;
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

let print_player_stats (player : Player.t) =
  let font_size = 10 in
  let spacing = font_size * 4 / 5 in
  let xindent = width * 1 / 32 in
  let yindent = font_size + spacing in
  draw_text
    ("Current Player: " ^ Player.get_name player)
    xindent yindent font_size text_color;
  draw_text
    ("Current Balance: " ^ string_of_int (Player.account player))
    xindent (yindent * 2) font_size text_color;
  draw_text
    ("Current Location: "
    ^ string_of_int (Position.get_index (Player.current_location player)))
    xindent (yindent * 3) font_size text_color;
  let properties = Player.get_owned_properties player in
  draw_text "Properties Owned:" xindent (yindent * 4) font_size text_color;
  for x = 0 to List.length properties - 1 do
    draw_text
      (Position.get_name (List.nth properties x))
      (xindent * 3 / 2)
      (yindent * (x + 5))
      font_size fine_text_color
  done

let player_position player1 player2 =
  (* draw player 1: *)
  let color1 = Player.get_color player1 in
  let position1 = Position.get_index (Player.current_location player1) in
  let p_radius = float_of_int grid_width *. p_size in
  let p1_leftx = fst board_tl + (start_prop / 4) in
  let p1_rightx =
    fst board_tl + start_prop + (grid_width * 9) + (start_prop / 4)
  in
  let p1_topy = snd board_tl + (start_prop / 4) in
  let p1_bottomy =
    snd board_tl + start_prop + (9 * grid_width) + (start_prop / 4)
  in
  if position1 > 30 then
    (* positions 39 - 31: *)
    draw_circle p1_rightx
      (snd board_tr + start_prop
      + (grid_width * (position1 - 30))
      - (grid_width / 2))
      p_radius color1
  else if position1 > 20 then
    (* player at top edge, positions 30 - 21 *)
    let p1_y = snd board_tl + (start_prop / 4) in
    if position1 == 30 then
      (* position 30: top right corner *)
      draw_circle p1_rightx p1_y p_radius color1
    else
      (* positions 29 - 21: *)
      draw_circle
        (fst board_bl + start_prop
        + (grid_width * (position1 - 20))
        - (grid_width / 2))
        p1_y p_radius color1
  else if position1 > 10 then
    (* player at left edge; positions 20 - 11 *)
    if position1 == 20 then
      (* position 20: top left corner *)
      draw_circle p1_leftx p1_topy p_radius color1
    else
      (* positions 19 - 11: *)
      draw_circle p1_leftx
        (snd board_tr + start_prop
        + (grid_width * (20 - position1))
        - (grid_width / 2))
        p_radius color1
  else if position1 == 10 then
    (* position 10: bottom left corner *)
    draw_circle p1_leftx p1_bottomy p_radius color1
  else if position1 > 0 then
    (* positions 9 - 1: *)
    draw_circle
      (fst board_bl + start_prop
      + (grid_width * (10 - position1))
      - (grid_width / 2))
      p1_bottomy p_radius color1
  else
    (* position 0: bottom right corner (start) *)
    draw_circle p1_rightx p1_bottomy p_radius color1;
  (* draw player 2: *)
  let color2 = Player.get_color player2 in
  let position2 = Position.get_index (Player.current_location player2) in
  (*let p_radius = float_of_int grid_width *. p_size in*)
  let p2_leftx = fst board_tl + (start_prop * 3 / 4) in
  let p2_rightx =
    fst board_tl + start_prop + (grid_width * 9) + (start_prop * 3 / 4)
  in
  let p2_topy = snd board_tl + (start_prop * 3 / 4) in
  let p2_bottomy =
    snd board_tl + start_prop + (9 * grid_width) + (start_prop * 3 / 4)
  in
  if position2 > 30 then
    (* positions 39 - 31: *)
    draw_circle p2_rightx
      (snd board_tr + start_prop
      + (grid_width * (position2 - 30))
      - (grid_width / 2))
      p_radius color2
  else if position2 > 20 then
    (* player at top edge, positions 30 - 21 *)
    let p2_y = snd board_tl + (start_prop * 3 / 4) in
    if position2 == 30 then
      (* position 30: top right corner *)
      draw_circle p2_rightx p2_y p_radius color1
    else
      (* positions 29 - 21: *)
      draw_circle
        (fst board_bl + start_prop
        + (grid_width * (position2 - 20))
        - (grid_width / 2))
        p2_y p_radius color2
  else if position2 > 10 then
    (* player at left edge; positions 20 - 11 *)
    if position2 == 20 then
      (* position 20: top left corner *)
      draw_circle p2_leftx p2_topy p_radius color2
    else
      (* positions 19 - 11: *)
      draw_circle p2_leftx
        (snd board_tr + start_prop
        + (grid_width * (20 - position2))
        - (grid_width / 2))
        p_radius color2
  else if position2 == 10 then
    (* position 10: bottom left corner *)
    draw_circle p2_leftx p2_bottomy p_radius color2
  else if position2 > 0 then
    (* positions 9 - 1: *)
    draw_circle
      (fst board_bl + start_prop
      + (grid_width * (10 - position2))
      - (grid_width / 2))
      p2_bottomy p_radius color2
  else
    (* position 0: bottom right corner (start) *)
    draw_circle p2_rightx p2_bottomy p_radius color2

let rec draw_player_window player1 player2 =
  (* if Raylib.window_should_close () then Raylib.close_window () else*)
  begin_drawing ();
  clear_background Color.white;
  draw_board_base ();
  player_position player1 player2;
  print_player_stats player1;
  print_insns "Press B to return to the terminal" fine_text_color;
  end_drawing ();
  if is_key_down Key.B = false then draw_player_window player1 player2

let rec draw_intro () =
  begin_drawing ();
  clear_background Color.white;
  (* Draw board base: *)
  draw_board_base ();
  (* Add text: *)
  print_insns "Press the space bar to continue" fine_text_color;
  end_drawing ();
  if is_key_down Key.Space == false then draw_intro ()

let rec draw_exit () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else begin_drawing ();
  clear_background Color.white;
  center_text 1 1 "Thanks For Playing!" text_color;
  print_insns "Press escape to leave" text_color;
  end_drawing ();
  draw_exit ()

(* initial window, all windows must be recursive*)
let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    (* call begin and and drawing between each drawing *)
    begin_drawing ();
    clear_background Color.white;
    (*if is_key_down Key.Space then check_start ();*)
    if is_key_down Key.Space then draw_intro ();
    (* call at the end of each drawing *)
    end_drawing ();
    loop ()

(*let () = setup |> loop*)
