type t = { current : int }

let init = { current = 0 }
let position b = b.current
let move_to b i = { current = b.current + i }
