type t = {
  name : string;
  board : Board.t;
  current : int;
  account : Account.t
}

let new_player s = { name = s; board = Board.init; current = 0 ; account = Account.init}
let get_board p = p.board
let current_location p = p.current;;

Random.self_init ()

let move p x =
  if x >= 36 then
    {
      name = p.name;
      board = Board.move_to p.board x;
      current = current_location p + x - 36;
      account = p.account;
    }
  else
    {
      name = p.name;
      board = Board.move_to p.board x;
      current = current_location p + x;
      account = p.account;
    }

  let buy p x =
    {
      name = p.name;
      board = p.board;
      current = p.current;
      account = Account.pay x p.account;
    }
