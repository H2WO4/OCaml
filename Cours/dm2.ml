type frac = {n: int; d: int}



(* Exercice 1 *)

let pgcd a b =
  let rec aux c d =
    match d with
    | 0 -> c
    | _ -> aux d (c mod d)
  in
  aux (abs a) (abs b)


let simplifie a =
  match a with
  | {n = x; d = y} when x * y > 0 -> let k = pgcd x y in {n = abs x / k; d = abs y / k}
  | {n = x; d = y} -> let k = pgcd x y in {n = x / k; d = y / k}


let print_frac a =
  match a with
  | {n = x; d = 1} -> Printf.printf "%d" x
  | {n = x; d = y} -> Printf.printf "%d/%d" x y

let string_of_frac a =
  match a with
  | {n = x; d = 1} -> string_of_int x
  | {n = x; d = -1} -> string_of_int ~-x
  | {n = x; d = y} when x * y < 0 -> string_of_int ~-(abs x) ^ "/" ^ string_of_int (abs y)
  | {n = x; d = y} -> string_of_int (abs x) ^ "/" ^ string_of_int (abs y)


let ( ++ ) a b = simplifie {n = a.n * b.d + b.n * a.d; d = a.d * b.d}

let ( -- ) a b = simplifie {n = a.n * b.d - b.n * a.d; d = a.d * b.d}

let ( ~-- ) a = simplifie {n = -a.n ; d = a.d}

let ( **: ) a b = simplifie {n = a.n * b.n; d = a.d * b.d} (* On évite de remplacer l'opérateur puissance de base de OCaml *)

let ( // ) a b = simplifie {n = a.n * b.d; d = a.d * b.n}


let f s =
  match String.split_on_char '/' s with
  | x :: y :: [] -> {n = int_of_string x; d = int_of_string y}
  | x :: [] -> {n = int_of_string x; d = 1}
  | _ -> failwith "invalid string"


let eval x p =
  let e = (10. ** (float_of_int p)) in
  print_float (Float.round ((float_of_int x.n /. float_of_int x.d) *. e) /. e)



(* Exercice 2 *)

let degre p = List.length p - 1


let affiche p =
  let rec aux l n =
    match l with
    | hd :: mi :: [] when n = (degre p) -> string_of_frac hd ^ "X" ^ aux [mi] (n-1)
    | hd :: mi :: [] when hd = f"0" -> aux [mi] 0
    | hd :: mi :: [] when hd = f"1" -> "+X" ^ aux [mi] 0
    | hd :: mi :: [] when hd > f"0" -> "+" ^ string_of_frac hd ^ "X" ^ aux [mi] 0
    | hd :: mi :: [] -> string_of_frac hd ^ "X" ^ aux [mi] 0
    | hd :: [] when n = (degre p) -> string_of_frac hd
    | hd :: [] when hd = f"0" -> ""
    | hd :: [] when hd > f"0" -> "+" ^ string_of_frac hd
    | hd :: [] -> string_of_frac hd
    | hd :: tl when n = (degre p) -> string_of_frac hd ^ "X^" ^ (string_of_int n) ^ aux tl (n-1)
    | hd :: tl when hd = f"0" -> aux tl (n-1)
    | hd :: tl when hd = f"1" -> "+X^" ^ (string_of_int n) ^ aux tl (n-1)
    | hd :: tl when hd > f"0" -> "+" ^ string_of_frac hd ^ "X^" ^ (string_of_int n) ^ aux tl (n-1)
    | hd :: tl -> string_of_frac hd ^ "X^" ^ (string_of_int n) ^ aux tl (n-1)
    | [] -> ""
  in
  print_string (aux (List.rev p) (degre p))


let rec image x p =
  match p with
  | hd :: [] -> hd ++ x
  | hd :: tl -> hd ++ x **: (image x tl)
  | [] -> f"0"


let clean p =
  let rec aux q =
    match q with
    | hd :: tl -> if hd = f"0" then aux tl else q
    | [] -> []
  in
  List.rev (aux (List.rev p))
(* Fonction en plus pour enlever les zéros qui traînent *)

let produitXn p n = List.init n (fun _ -> f"0") @ p


let somme p1 p2 =
  let a, b = List.length p1, List.length p2 in
  let len = max a b in
  let p3 = p1 @ List.init (len - a) (fun _ -> f"0") in
  let p4 = p2 @ List.init (len - b) (fun _ -> f"0") in
  (* On s'assure juste de mettre les polynômes à la même taille *)
  clean (List.map2 (fun x y -> x ++ y) p3 p4)

let opposee p = List.map (fun x -> ~--x) p

let difference p1 p2 = somme p1 (opposee p2)


let derive p =
  let rec aux l n =
    match l with
    | _ :: tl when n = 0 -> aux tl (n+1)
    | hd :: tl -> (f (string_of_int n)) **: hd :: aux tl (n+1)
    | [] -> []
  in
  aux p 0


let produit_nb p a = List.map (fun x -> x **: a) p


let produit p1 p2 = List.fold_left (somme) [] (List.init (List.length p1) (fun x -> produitXn (produit_nb p2 (List.nth p1 x)) x))


let rec coeff_dom (p : frac list) = 
  match p with
  | hd :: [] -> hd
  | _ :: tl -> coeff_dom tl
  | [] -> f"0"


let rec reste p3 p4 acc =
  match degre p3, degre p4 with
  | x, y when x >= y -> reste (clean (difference p3 (produitXn (produit p4 [(coeff_dom p3) // (coeff_dom p4)]) acc))) p4 (acc- 1)
  | _, _ -> p3
(* Je sais pas comment ma fonction marche, mais apparemment, elle donne le bon résultat ... *)
