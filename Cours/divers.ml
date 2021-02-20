let zone x y =
  match x, y with
  | _, _ when y < (x ** 2.) -> 'c'
  | _, _ when y > (sqrt x) -> 'a'
  | _, _ -> 'b'

let () = Printf.printf "%c" (zone 0.5 1.)

let newAnd a b =
  match 