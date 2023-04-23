type command =
  | Roll
  | Purchase
  | EndTurn
  | Quit

exception Empty
exception Malformed

let parse str =
  let slist = String.split_on_char (char_of_int 32) str in
  if slist = [] then raise Empty
  else
    let rec elem s acc =
      match s with
      | [] -> acc
      | h :: t1 -> if h = "" then elem t1 acc else elem t1 (h :: acc)
    in
    let s = elem slist [] in
    let p = List.rev s in
    match p with
    | [] -> raise Empty
    | h :: t2 ->
        if h = "quit" then Quit
        else if h = "roll" then Roll
        else if h = "purchase" then Purchase
        else if h = "end" then EndTurn
        else raise Malformed
