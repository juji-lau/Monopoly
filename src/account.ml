type t = { current : int }

let init = { current = 1500 }
<<<<<<< HEAD
let current a = a.current

let pay i a =
  if a.current - i >= 0 then { current = a.current - i } else raise Player.Broke

let receive i a = { current = a.current + i }
=======

exception Broke

let current a = a.current

let pay i a =
  if a.current - i >= 0 then { current = a.current - i } else raise Broke

let recieve i a = { current = a.current + i }
>>>>>>> e1c094dbd2c1ea62abcea831fe28120cb09c7d8e
