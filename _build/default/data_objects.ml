let rec last_elem l =
  match l with
  | [] -> failwith "last_elem called on empty list"
  | [x] -> x
  | _ :: tl -> last_elem tl
let all_but_last l =
  let rec aux a acc =
    match a with
    | [] -> failwith "all_but_last called on empty list"
    | [_] -> acc
    | hd :: tl -> aux tl (hd :: acc)
  in
  (aux l []) |> List.rev


class ['a] stack = object (_)
  val mutable values = ([] : 'a list)
  
  method push x =
    values <- x :: values
  
  method pop =
    let out = List.hd values in
    values <- List.tl values;
    out
  
  method view =
    List.hd values
  
  method size =
    List.length values
end
class ['a] queue = object (_)
  val mutable values = ([] : 'a list)

  method push x =
    values <- x :: values

  method pop =
    let out = last_elem values in
    values <- all_but_last values;
    out
  
  method view =
    last_elem values
  
  method size =
    List.length values
end
class ['a] tower = object (_)
  val mutable values = ([] : 'a list)

  method push x =
    let rec aux x l =
      match l with
      | [] -> [x]
      | [y] -> if x > y then y :: [x] else x :: [y]
      | hd :: _ when x < hd -> x :: l
      | hd :: tl -> hd :: (aux x tl)
    in
    values <- (aux x values);
  
  method pop =
    let out = List.hd values in
    values <- List.tl values;
    out
  
  method view =
    List.hd values
    
  method size =
    List.length values
end
class ['a] pile = object (_)
  val mutable values = ([] : 'a list)

  method push x =
    let rec aux x l =
      match l with
      | [] -> [x]
      | [y] -> if x < y then [y; x] else [x; y]
      | hd :: _ when x > hd -> x :: l
      | hd :: tl -> hd :: (aux x tl)
    in
    values <- (aux x values)
  
  method pop =
    let out = List.hd values in
    values <- List.tl values;
    out
  
  method view =
    List.hd values
    
  method size =
    List.length values
end


let empty_struct o =
  while o#size > 0 do
    ignore (o#pop)
  done
let fold_struct f b o =
  let rec aux obj func acc =
    match obj#pop with
    | x when obj#size == 0 -> func acc x
    | x -> aux obj func (func acc x)
  in
  aux o f b
let rec cycle_queue (o : 'a queue) x =
  match x with
  | 0 ->  o#push o#pop
  | _ ->  o#push o#pop; cycle_queue o (x-1)
let ( >- ) a s = s#push a
let ( ~< ) s = s#pop

let newStack = new stack;;

for i = 1 to 10 do
  i >- newStack
done
let () = let x = ~< newStack in print_int x