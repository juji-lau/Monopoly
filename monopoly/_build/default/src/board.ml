type t = {
  position : int
}
let init = {position = 0}

let position b = b.position

let move_to b i = {
  position = b.position + i
}