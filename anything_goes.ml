let range a b =
  let rec aux min max acc =
    match max < min with
    | true -> acc
    | false -> aux min (max-1) (max :: acc)
  in
  match a > b with
  | true -> List.rev (aux b a [])
  | false -> aux a b []

let range_oper a b f =
  let rec aux min max func acc =
    match max < min with
    | true -> acc
    | false -> aux (func min) max func (min :: acc)
  in
  match a > b with
  | true -> aux b a f []
  | false -> List.rev (aux a b f [])

let repeat a b =
  let rec aux elem times acc =
    match times with
    | 0 -> acc
    | _ -> aux elem (times-1) (elem :: acc)
  in
  aux a b []

let factorial a =
  List.fold_left ( + ) 0 (range 1 a)

let power a b =
  List.fold_left ( * ) 1 (repeat a b)

let rec last_elem l =
  match l with
  | [] -> failwith "last_elem called on empty list"
  | [x] -> x
  | _ :: tl -> last_elem tl

let rec max_elem l =
  match l with
  | [] -> failwith "max_elem called on empty list"
  | [x] -> x
  | hd :: tl -> max hd (max_elem tl)

let rec min_elem l =
  match l with
  | [] -> failwith "min_elem called on empty list"
  | [x] -> x
  | hd :: tl -> min hd (min_elem tl)

let rec duplicate l =
  match l with
  | [] -> []
  | hd :: tl -> hd :: hd :: duplicate tl

let list_divisors a =
  let rec aux n m acc =
    match n mod m with
    | 0 when m = 1 -> 1 :: n :: acc
    | 0 -> aux n (m-1) (m :: n/m :: acc)
    | _ -> aux n (m-1) acc
  in
  List.sort compare (aux a (truncate (sqrt (float a))) [])

let is_prime a =
  let rec aux n m =
    match n mod m with
    | 0 when m = 1 -> true
    | 0 -> false
    | _ -> true && aux n (m-1)
  in
  aux a (truncate (sqrt (float a)))

let rec gcd a b =
  match b with
  | 0 -> a
  | _ -> gcd b (a mod b)

let coprime a b =
  match gcd a b with
  | 1 -> true
  | _ -> false

let totient a =
  let rec aux n m acc =
    match m with
    | 1 -> m :: acc
    | _ when (coprime n m) -> aux n (m-1) (m :: acc)
    | _ -> aux n (m-1) acc
  in
  match a with
  | x when x <= 0 -> 0
  | 1 -> 1
  | _ -> List.length (aux a (a-1) [])


let () = List.iter (Printf.printf "%d ") (range_oper 2 1000 (( * ) 2))