type 'a binTree =
  | Branch of 'a * 'a binTree * 'a binTree
  | Leaf of 'a



let rec toStringCharTree (t : char binTree) =
  match t with
  | Leaf x -> String.make 1 x
  | Branch (x, l, r) -> String.make 1 x ^ "(" ^ toStringCharTree l ^ " " ^ toStringCharTree r ^ ")"


let rec toStringIntTree (t : int binTree) =
  match t with
  | Leaf x -> string_of_int x
  | Branch (x, l, r) -> string_of_int x ^ "(" ^ toStringIntTree l ^ " " ^ toStringIntTree r ^ ")"


let countLeaves t =
  let rec aux t acc = 
    match t with
    | Leaf _ -> 1
    | Branch (_, l, r) -> (aux l acc) + (aux r acc)
  in
  aux t 0


let rec randomTree n x =
  match Random.int 10 with
  | m when n > 0 && m > 2 -> Branch(Random.int x, randomTree (n-1) x, randomTree(n-1) x)
  | _ -> Leaf (Random.int x)



let tree = Branch ('A', Leaf 'B', Branch ('C', Leaf 'D', Leaf 'E'))
let tree2 = randomTree 4 100

let () = print_string (toStringIntTree tree2)
let () = print_newline ()
let () = print_int (countLeaves tree2)