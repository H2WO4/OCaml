(* Exercice 1 *)

let partition l =
  let x = List.length l / 2 in
  let rec aux m acc =
    match m with
    | hd :: tl when acc = x -> ([hd], tl)
    | hd :: tl -> let a, b = aux tl (acc + 1) in ((hd :: a), b)
    | [] -> ([], [])
  in aux l 1

let rec fusion l1 l2 =
  match l1, l2 with
  | hd1 :: tl1, hd2 :: _ when hd1 < hd2 -> hd1 :: (fusion tl1 l2)
  | hd1 :: _, hd2 :: tl2 -> hd2 :: (fusion l1 tl2)
  | _, [] -> l1
  | [], _ -> l2

let rec tri_partition_fusion l =
  let a, b = partition l in
  let x = if List.length a <= 1 then a else tri_partition_fusion a in
  let y = if List.length b <= 1 then b else tri_partition_fusion b in
  fusion x y


(* Exercice 2 *)

let rec fibo_naif a =
  match a with
  | 0 | 1 -> 1
  | _ -> fibo_naif (a-1) + fibo_naif (a-2)
(* L'algorithme est O(2 ** n), très lent *)

let fibo_suites a =
  let rec u x =
    match x with
    | 0 -> 1
    | _ -> v (x-1)
  and v x =
    match x with
    | 0 -> 1
    | _ -> u (x-1) + v (x-1)
  in u a
(* Complexité 0(n) *)
  
let carre a = a * a
(* F_2n = F_n ** 2 + F_n-1 ** 2 *)
(* F_2n+1 = F_n * F_n+1 + F_n-1 * F_n = F_n * (F_n-1 + F_n+1) *)
let rec fibo_DpR a =
  match a with
  | 0 | 1 -> 1
  | _ when a mod 2 = 0 -> carre (fibo_DpR (a / 2)) + carre (fibo_DpR ((a-2) / 2))
  | _ -> fibo_DpR (a / 2) * (fibo_DpR ((a-3) / 2) + fibo_DpR ((a+1) / 2))

let temps f b = Array.init b (fun x -> let y = Sys.time () in f x; Sys.time () -. y)

let write_cvs a =
  let oc = open_out "td5.cvs" in
  let b = Array.length a in 
  let c = Array.init (Array.length a.(0)) (fun x -> Array.init b (fun y -> string_of_float a.(y).(x))) in
  Array.iter (fun x -> output_string oc (Array.fold_right (fun x y -> if y = "" then x else x ^ ";" ^ y) x "" ^ "\n")) c;
  close_out oc


(* Exercice 3 *)

let print_int_array arr = Array.iter (fun x -> Array.iter (Printf.printf "%2d ") x; print_newline ()) arr

let power a b = List.fold_left ( * ) 1 (List.init b (fun _ -> a))

let cree k =
  let n = power 2 k in
  let arr = Array.init n (fun _ -> Array.init n (fun _ -> 0)) in
  for i = 0 to Array.length arr - 1 do
    for j = 0 to Array.length arr - 1 do
      if i = 0 then
        if j = 0 then arr.(i).(j) <- Random.int 7 + 1
        else arr.(i).(j) <- arr.(i).(j-1) + Random.int 7 + 1
      else
        if j = 0 then arr.(i).(j) <- arr.(i-1).(j) + Random.int 7 + 1
        else arr.(i).(j) <- (max arr.(i).(j-1) arr.(i-1).(j)) + Random.int 7 + 1
    done
  done; arr

(* Si v > x, alors on peut éliminer toutes les valeurs {M(a, b) / a <= 2n-1 et b <= 2n-1} *)
(* Si v < y, alors on peut éliminer toutes les valeurs {M(a, b) / a >= 2n et b >= 2n} *)

(* On appelle donc la fonction sur les sous-tableau suivants: *)
(* Si x < v < y -> {M(a, b) / a >= 2n et b <= 2n-1} ainsi que {M(a, b) / a <= 2n-1 et b >= 2n} *)
(* Si v < x -> {M(a, b) / a <= 2n-1 et b <= 2n-1} *)
(* Si y < v -> {M(a, b) / a >= 2n et b >= 2n} *)

(* Dans le pire des cas, la valeur se trouve dans le coin haut-droit ou bas-gauche, et prendra 2log_2(n) voir 3log_2(n) comparaisons à trouver *)
(* L'alogorithme est donc O(log(n)) *)

let rec cherche_tableau arr e =
  let mid = Array.length arr / 2 in
  let x, y = arr.(mid-1).(mid-1), arr.(mid).(mid) in
  if e = x || e = y then true else
  if mid = 1 then e = arr.(0).(1) || e = arr.(1).(0) else
  if e < x then cherche_tableau (Array.init mid (fun x -> Array.init mid (fun y -> arr.(x).(y)))) e else false ||
  if e > y then cherche_tableau (Array.init mid (fun x -> Array.init mid (fun y -> arr.(mid+x).(mid+y)))) e else false ||
  cherche_tableau (Array.init mid (fun x -> Array.init mid (fun y -> arr.(mid+x).(y)))) e ||
  cherche_tableau (Array.init mid (fun x -> Array.init mid (fun y -> arr.(x).(mid+y)))) e


(* Exercice 4 *)

let max_i l = Array.fold_left max Int.min_int l
(* n comparaison sont effectués, l'algorithme est donc O(n) *)

(* n appels à max sont effectués, la version "diviser pour régner" n'a aucun avantage dans ce cas *)


(* Exercice 5 *)

let rec power_rec a b =
  match b with
  | 0 -> 1
  | 1 -> a
  | 2 -> a * a
  | _ when b mod 2 = 0 -> let c = power_rec a (b/2) in c * c
  | _ -> let c = power_rec a ((b-1)/2) in a * c * c

let rec power_rec_mod a b n =
  match b with
  | 0 -> 1
  | 1 -> a mod n
  | 2 -> (a * a) mod n
  | _ when b mod 2 = 0 -> let c = power_rec_mod a (b/2) n in (c * c) mod n
  | _ -> let c = power_rec_mod a ((b-1)/2) n in (a * c * c) mod n
