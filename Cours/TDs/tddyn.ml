(* Exercice 1 *)

let glouton pieces somme =
  let n = Array.length pieces in
  let out, rsomme = Array.make n 0, ref somme in
  for i=0 to n-1 do
    let a = !rsomme / pieces.(i) in
    out.(i) <- a;
    rsomme := !rsomme - (pieces.(i) * a)
  done; out


(* Exercice 2 *)

let rec min_elem l =
  match l with
  | [] -> failwith "min_elem called on empty list"
  | [x] -> x
  | hd :: tl -> min hd (min_elem tl)

let recursif pieces somme =
  let pieces = Array.to_list pieces in
  let rec aux p acc =
    match p with
    | 0 -> 0, []
    | n -> match min_elem (List.map (fun x -> match aux (p-x) [] with | a, b -> a, x :: b) (List.filter (fun x -> x <= n) pieces)) with
      | a, b -> 1 + a, b @ acc
  in aux somme []


(* Exercice 3 *)
(* L'algorithme a une complexité temporelle trop importante, ainsi, nous ne pouvons avoir de résultat *)


(* Exercice 4, 5, 6 *)

let dynamique pieces somme =
  let pieces = Array.to_list pieces in
  let mat = Array.make (somme+1) (0, []) in
  let aux n =
    match n with
    | 0 -> 0, []
    | n -> match min_elem (List.map (fun x -> match mat.(n-x) with | a, b -> a, x :: b) (List.filter (fun x -> x <= n) pieces)) with
      | a, b -> 1 + a, b 
  in
  for i=0 to somme do
    mat.(i) <- aux i
  done; mat.(somme)


let () = match dynamique [|50000; 20000; 10000; 5000; 2000; 1000; 500; 200; 100; 50; 20; 10; 5; 2; 1|] 2751938 with
  | a, b -> List.iter (Printf.printf "%d ") b;