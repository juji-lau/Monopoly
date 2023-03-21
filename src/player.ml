type t = {
  name : string;
  board : Board.t;
  current : int;
}

let new_player s = { name = s; board = Board.init; current = 0 }
let current_location p = p.current;;

Random.self_init ()

let move p =
  let x = current_location p + Random.int 12 in
  if x >= 36 then { name = p.name; board = p.board; current = x - 36 }

let current_location p = p.current

let move p dice_roll =
  let x = current_location p + dice_roll in
  if x > 39 then { name = p.name; board = p.board; current = x - 40 }
  else { name = p.name; board = p.board; current = x }
