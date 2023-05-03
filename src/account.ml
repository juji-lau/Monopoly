type t = { current : int }

let init = { current = 1500 }
let current a = a.current

let pay i a =
  if a.current - i >= 0 then { current = a.current - i } else raise Player.Broke

let receive i a = { current = a.current + i }
