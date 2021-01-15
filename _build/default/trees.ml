type 'a binTree =
  | Branch of 'a * 'a binTree * 'a binTree
  | Leaf of 'a


let rec toStringTree (t : char binTree) =
  match t with
  | Leaf x -> String.make 1 x
  | Branch (x, l, r) -> String.make 1 x ^ "(" ^ toStringTree l ^ " " ^ toStringTree r ^ ")"

let tree = Branch ('A', Leaf 'B', Branch ('C', Leaf 'D', Leaf 'E'))

let () = print_string (toStringTree tree)