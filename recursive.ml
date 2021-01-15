let range a b =
  let rec aux a b acc =
    match b < a with
    | true -> acc
    | false -> aux a (b-1) (b :: acc)
  in
  match a > b with
  | true -> List.rev (aux b a [])
  | false -> aux a b []

let repeat a b =
  let rec aux elem times acc =
    match times with
    | 0 -> acc
    | _ -> aux elem (times-1) (elem :: acc)
  in
  aux a b []



let rec fibonnacci a =
  match a with
  | 0 | 1 -> 1
  | x -> fibonnacci (x-1) + fibonnacci (x-2)

let () = List.iter (Printf.printf "%d ") (List.map fibonnacci (range 1 10))