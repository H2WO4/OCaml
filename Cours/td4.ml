let rec index e l =
  match l with
  | x :: _ when x = e -> 0
  | [] -> failwith "Element not present"
  | _ :: tl -> 1 + (index e tl)


let maximum l = List.fold_left (fun x y -> if x < y then y else x) (List.hd l) l 


let rec supprime n l =
  match l with
  | _ :: tl when n = 0 -> supprime (n-1) tl
  | hd :: tl -> hd :: (supprime (n-1) tl)
  | [] -> []

let rec supprime_elt e l =
  match l with
  | hd :: tl when hd = e -> supprime_elt e tl
  | hd :: tl -> hd :: (supprime_elt e tl)
  | [] -> []

let supprime_doublon l =
  let rec aux li acc =
    match li with
    | hd :: tl when List.mem hd acc -> aux tl acc
    | hd :: tl -> hd :: (aux tl (hd :: acc))
    | [] -> [] 
  in
  aux l []


let applatir l = List.fold_left (@) [] l


let (<~) (a, b) (c, d) = a < c || (a = c && b <= d)


let rec (<~:) l1 l2 =
  match l1, l2 with
  | [], _ -> failwith "comparaison impossible"
  | _, [] -> failwith "comparaison impossible"
  | [x], [y] -> x <= y
  | hd1 :: tl1, hd2 :: tl2 when hd1 = hd2 -> tl1 <~: tl2
  | hd1 :: tl1, hd2 :: tl2 -> (hd1, tl1) <~ (hd2, tl2)
  

let rec selection f l =
  match l with
  | [] -> []
  | hd :: tl when f hd -> hd :: selection f tl
  | _ :: tl -> selection f tl


let rec intervalle_entier a b =
  match a < b with
  | true -> a :: intervalle_entier (a+1) b
  | false -> b :: []


let () = Printf.printf "%b " ([1; 4; 1; 4] <~: [1; 3; 3; 1])

