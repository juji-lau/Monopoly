type t = {
  name : string;
  board : Board.t;
  current : int;
}

let new_player s = { name = s; board = Board.init; current = 0 }
let current_location p = p.current

let move p =
  let x = current_location p + Random.int 12 in
  if x >= 36 then { name = p.name; board = p.board; current = x - 36 }
  else { name = p.name; board = p.board; current = x }
