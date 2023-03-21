type t = { position : int }

let init = { position = 0 }
let position b = b.position

let move_to b i =
  if b.position + i <= 36 then { position = b.position + i }
  else { position = b.position + i - 36 }
