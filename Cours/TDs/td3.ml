let rec palindrome str =
  match String.length str with
  | 1 -> true
  | 2 -> str.[0] = str.[1]
  | x -> str.[0] = str.[x - 1] && palindrome (String.sub str 1 (x-2))

let palindrome2 str =
  let n = String.length str in
  let rec aux index =
    match index with
    | x when (2 * x) + 1 = n -> true
    | x when 2 * x = n -> str.[x] = str.[n-x-1]
    | x -> str.[x] = str.[n-x-1] && aux (x+1)
  in aux 0

let palindrome3 str =
  let n = String.length str in
  n <= 1 || str.[0] = str.[n-1] && palindrome (String.sub str 1 (n-2))


let est_trie arr = 
  let rec aux lst =
    match lst with
    | [] -> true
    | _ :: [] -> true
    | hd :: mid :: tl when hd <= mid -> aux (mid::tl)
    | _ -> false
  in aux (Array.to_list arr)


let couple_premiers n =
  let coprime a b =
    let rec gcd a b =
      match b with
      | 0 -> a
      | _ -> gcd b (a mod b)
    in match gcd a b with
    | 1 -> true
    | _ -> false
  in let rec aux m o acc =
    match coprime m o with
    | true when m = n && o = n -> acc+1
    | true when m = n -> aux 1 (o+1) (acc+1)
    | true -> aux (m+1) o (acc+1)
    | false when o = n -> acc
    | false when m = n -> aux 1 (o+1) acc
    | false -> aux (m+1) o acc
  in aux 1 1 0


let decompose n =
  let list_divisors a =
    let is_prime a =
      let rec aux n m =
        match n mod m with
        | 0 when m = 1 -> true
        | 0 -> false
        | _ -> aux n (m-1)
      in aux a (a |> float |> sqrt |> truncate)
    in let rec aux n m acc =
      match n mod m with
      | 0 when m = 1 -> acc
      | 0 -> aux n (m-1) (m :: n/m :: acc)
      | _ -> aux n (m-1) acc
    in List.filter (is_prime) (aux a (a |> float |> sqrt |> truncate) [])
  in let rec aux m acc =
    let div = list_divisors m in
    let x = ref m in
    if div = [] then acc
    else begin
      List.iter (fun y -> x := !x / y) div;
      aux !x acc@div
    end
  in List.iter (Printf.printf "%d ") (List.sort compare (aux n []))


let rec mccarthy n =
  match n > 100 with
  | true -> n - 10
  | false -> mccarthy (mccarthy (n+11))
(* Elle renvoie 91 ssi n <= 101 *)


let rec terme f a n =
  match n with
  | 0 -> a
  | _ -> terme f (f a) (n-1)


let rec permut n =
  match n with
  | 0 -> [[0]]
  | 1 -> [[1]]
  | _ -> [n] :: List.flatten (List.flatten (List.init (n/2) (fun x -> List.map (fun y -> List.map (fun z -> List.flatten [y; z]) (permut (x+1))) (permut (n-x-1)))))


let rec dichotomie f a b e =
  if f a < e && -.e < f a then
    a
  else
    let n = (b-.a)/.2. in
    if (f a) -. (f b) < 0. then
      dichotomie f a n e
    else
      dichotomie f n b e


let () = Printf.printf "%f" (dichotomie (fun x -> 2.*.x) (-.3.) 3. 0.01)