type t = {
  board : Position.square list;
  initial : Position.square;
  current_int : int;
}

exception UnimplementedBoard

let init =
  let (b : Position.square list) = Position.new_board in
  match b with
  | [] -> raise UnimplementedBoard
  | h :: _ -> { board = b; initial = h; current_int = 0 }

let get_initial (b : t) : Position.square = b.initial

(* let position b = b.current *)
(* let position_int b = b.current_int *)

(* let move_to b i = let x = b.current_int + i in let lst = b.board in if x >=
   40 then let curr = List.nth lst (x - 40) in { board = b.board; current =
   curr; current_int = x - 40 } else let curr = List.nth lst x in { board =
   b.board; current = curr; current_int = x } *)
