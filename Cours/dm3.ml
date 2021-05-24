(* Exercice 1 *)

let hamming_array a1 a2 =
  let len = Array.length a1 in
  if len <> Array.length a2 then failwith "Tableaux de longueurs différentes" else
  Array.fold_left (+) 0 (Array.init len (fun x -> if a1.(x) = a2.(x) then 0 else 1))

let hamming_list l1 l2 =
  let rec aux li1 li2 acc =
    match li1, li2 with
    | hd1 :: tl1, hd2 :: tl2 when hd1 = hd2 -> aux tl1 tl2 acc
    | hd1 :: tl1, hd2 :: tl2 -> aux tl1 tl2 (acc+1)
    | [], [] -> acc
    | _, _ -> failwith "Listes de longueurs différentes"
  in aux l1 l2 0


(* Exercice 2 *)
let compresser l =
  let rec aux m acc =
    match m, acc with
    | hd1 :: tl1, hd2 :: mi2 :: tl2 when hd1 = mi2 -> aux tl1 ((hd2 + 1) :: mi2 :: tl2)
    | hd1 :: tl1, _ -> aux tl1 (1 :: hd1 :: acc)
    | [], _ -> acc
  in aux (List.rev l) []

let rec decompresser l =
  match l with
  | hd :: mi :: tl -> (List.init hd (fun _ -> mi)) @ (decompresser tl)
  | [] -> []
  | _ -> failwith "Liste non conforme"


(* Exercice 3 *)

let joindre l sep = List.fold_left (fun x y -> if x = "" then y else x ^ sep ^ y) "" l

let scinder l sep =
  let rec aux l (sep : char) acc =
    match l, acc with
    | hd1 :: tl1, l2 when hd1 = sep -> aux tl1 sep ([] :: l2)
    | hd1 :: tl1, hd2 :: tl2 -> aux tl1 sep ((hd1 :: hd2) :: tl2)
    | _, _ -> acc
  in
  aux (List.rev (List.of_seq (String.to_seq l))) sep [[]]


(* Exercice 4 *)
(* 1) Le meilleur chemin est 5 -> -6 -> 3 -> -2 = 0 *)

let rec escalier esc =
  match esc with
  | hd :: mi :: [] -> hd + mi
  | hd :: mi :: tl -> hd + (max (escalier (mi :: tl)) (escalier tl))
  | hd :: [] -> hd
  | [] -> 0
