type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Nil

let node tr1 x tr2 = Node(tr1, x, tr2)

(* Exercice 1 *)
let rec hauteur tr =
  match tr with
  | Node (tr1, _, tr2) -> 1 + max (hauteur tr1) (hauteur tr2)
  | Nil -> 0

let rec equilibre tr =
  match tr with
  | Node (tr1, _, tr2) -> abs ((hauteur tr1) - (hauteur tr2)) <= 1 && equilibre tr1 && equilibre tr2
  | Nil -> true


(* Exercice 2 *)

let rec prefix_tree tr =
  match tr with
  | Node (tr1, x, tr2) -> [x] @ (prefix_tree tr1) @ (prefix_tree tr2)
  | Nil -> []

let rec infix_tree tr =
  match tr with
  | Node (tr1, x, tr2) -> (infix_tree tr1) @ [x] @ (infix_tree tr2)
  | Nil -> []

let rec postfix_tree tr =
  match tr with
  | Node (tr1, x, tr2) -> (postfix_tree tr1) @ (postfix_tree tr2) @ [x]
  | Nil -> []


(* Exercice 3 *)

let level_tree tr =
  let rec aux ar n =
    match ar with
    | Node (tr1, x, tr2) when n = 0 -> [x]
    | Node (tr1, x, tr2) -> aux tr1 (n-1) @ aux tr2 (n-1)
    | Nil -> []
  in
  (* List.iter (Printf.printf "%d ") (List.concat (List.init (hauteur tr) (fun x -> aux tr x))) *)
  List.concat (List.init (hauteur tr) (fun x -> aux tr x))

(* Exercice 4 *)

(* Les arbres a1 et a3 sont des abres binaires de recherche *)

let rec abr tr =
  match tr with
  | Nil -> true
  | Node (tr1, x, tr2) -> match tr1, tr2 with
    | Node (_, y1, _), Node (_, y2, _) -> x >= y1 && x <= y2 && abr tr1 && abr tr2
    | Nil, Node (_, y2, _) -> x <= y2 && abr tr2
    | Node (_, y1, _), Nil -> x >= y1 && abr tr1
    | Nil, Nil -> true;;


(* Exercice 5 *)



let tree_test = node (node (node (node Nil 5 Nil) 3 (node Nil 6 Nil)) 1 (node (node Nil 7 Nil) 4 Nil)) 0 (node Nil 2 Nil)
let () = List.iter (Printf.printf "%d ") (level_tree tree_test)