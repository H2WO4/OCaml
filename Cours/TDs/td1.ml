let puiss a b = int_of_float ((float_of_int a) ** (float_of_int b))

let factorielle a =
  let rec aux a acc =
    match a with
    | 0 | 1 -> acc
    | x -> aux (x - 1) (acc * x)
  in
  aux a 1

let multiple_if a =
  match a mod 3 with
  | 0 -> 1
  | _ -> 0

let multiple_expr a = 2 - (a mod 3) / 2

let print_array arr =
  let print_int_with_tab a =
    print_string ((string_of_int a) ^ "\t")
  in
  Array.iter (print_int_with_tab) arr

let etendue arr =
  let n = Array.length arr in
  Array.fold_right (max) arr n - Array.fold_right (min) arr n

let modale arr =
  let count a =
    let checkEquality b c =
      match b = c with
      | true -> 1
      | false -> 0
    in
    Array.fold_right (+) (Array.map (checkEquality a) arr) 0
  in
  let find_index a b =
    let rec aux a b i =
      let n = Array.length b in
      match b.(i) with
      | x when a = x -> i
      | _ when i < n-1  -> aux a b (i+1)
      | _ -> i
    in
    aux a b 0
  in
  arr.(find_index (Array.fold_right max (Array.map count arr) 0) arr)

let pascal n =
  print_array [|1|]; print_endline "";
  let rec aux arr n =
    let len = Array.length arr in
    match n with
    | 1 ->
      for i = 1 to len - 1 do
        arr.(len-i) <- (arr.(len-i-1) + arr.(len-i))
      done; Array.append arr [|1|]
    | _ ->
      for i = 1 to len - 1 do
        arr.(len-i) <- (arr.(len-i-1) + arr.(len-i))
      done; print_array (Array.append arr [|1|]); print_endline "";
      aux (Array.append arr [|1|]) (n-1)
  in
  aux [|1|] n

let find_chr txt need =
  let equal a b =
    match a = b with
    | true -> '1'
    | false -> '0'
  in
  String.index_from (String.map (equal need) txt) 0 '1'

let palindrome txt =
  let ltxt = List.of_seq (String.to_seq (String.lowercase_ascii txt)) in
   if List.rev ltxt = ltxt then
    true
   else
    false

let voyelles txt =
  let capitalizeVoyels c =
    match c with
    |'a'|'i'|'u'|'e'|'o'|'y' -> Char.uppercase_ascii c
    | _ -> c
  in
  let ltxt = List.of_seq (String.to_seq txt) in
  String.of_seq (List.to_seq (List.map capitalizeVoyels ltxt))

let () = Printf.printf "%s" (voyelles "abcdefghijklmnopqrstuvwxyz")