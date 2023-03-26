
type t = { 
  mutable current : int
}

let init = { current = 1500 }

exception Broke
let current a = a.current
let pay i a = if a.current - i >= 0 then 
  a.current <- a.current - i
else
  raise Broke

let recieve i a = a.current <- a.current + i