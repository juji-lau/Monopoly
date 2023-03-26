type t = {
  name : string;
  board : Board.t;
  current : int;
  account : Account.t;
  properties : Position.t list
}

let new_player s = { name = s; board = Board.init; current = 0 ; account = Account.init; properties = []}
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
      properties = []
    }
  else
    {
      name = p.name;
      board = Board.move_to p.board x;
      current = current_location p + x;
      account = p.account;
      properties = []
    }

  let buy p x prop =
    {
      name = p.name;
      board = p.board;
      current = p.current;
      account = Account.pay x p.account;
      properties = []
    }
  
  let recieve p x =
    {
      name = p.name;
      board = p.board;
      current = p.current;
      account = Account.recieve x p.account;
      properties = []
    }
