let (>-) = Stack.push
let (~<) = Stack.pop

let laby = [|
    13;  1;  3; 11; 13;  1;  1;  5;  5;  1;  3; 11; 13;  5;  3; 11;
     9;  2; 14;  8;  3; 14; 12;  5;  3; 14; 12;  6;  9;  5;  2; 10;
    14;  8;  3; 14; 12;  3;  9;  5;  0;  1;  5;  3;  8;  7;  8;  2;
    11; 14;  8;  7; 13;  2; 14; 13;  2; 14;  9;  0;  4;  7; 10; 14;
     8;  5;  2; 13;  5;  0;  3; 13;  2; 13;  6; 12;  1;  7; 12;  3;
    12;  3;  8;  1;  1;  2;  8;  5;  6; 11; 11; 11; 12;  7;  9;  2;
    13;  2; 14; 14; 10; 10;  8;  7; 11; 10;  8;  2;  9;  5;  6; 10;
     9;  6; 13;  3; 14; 10; 14;  9;  4;  4;  2; 14; 14; 13;  1;  6;
     8;  5;  7;  8;  3; 12;  1;  4;  7; 11; 14; 13;  3; 11; 12;  7;
    14; 11; 13;  2; 12;  5;  0;  5;  3;  8;  5;  1;  6; 12;  3; 11;
     9;  4;  7; 12;  3; 11; 10; 13;  6;  8;  7;  8;  1;  5;  4;  2;
    10; 11;  9;  5;  4;  2; 14;  9;  1;  2;  9;  2; 12;  1;  7; 10;
     8;  0;  4;  3;  9;  4;  5;  2; 10; 10; 10;  8;  7; 12;  3; 14;
    14; 14; 13;  6; 12;  3; 11; 14; 10; 14; 14; 14;  9;  7; 10; 11;
    11;  9;  7; 11; 13;  0;  4;  7; 12;  1;  5;  7; 10;  9;  0;  6;
    12;  4;  5;  4;  5;  4;  5;  7; 13;  4;  7; 13;  4;  6; 12;  7|]


(* Exercice 1 *)

let liste_to_pile l =
  let rec aux s li =
    match li with
    | hd :: tl -> hd >- s; aux s tl
    | [] -> s
  in
  aux (Stack.create ()) (List.rev l)


(* Exercice 2 *)

let remplis n a b =
  let rec aux s m =
    match m with
    | 0 -> s
    | _ -> (Random.int (1+b-a)) + a >- s; aux s (m-1)
  in
  aux (Stack.create ()) n

(* Le sommet est affiché en premier, le stack n'est pas conservé *)

let rec print_stack s =
  if Stack.length s = 0 then () else
  if Stack.length s = 1 then
    Printf.printf "%d" (Stack.top s)
  else
    let a = ~< s in print_stack s; Printf.printf " <- %d" a; a >- s


(* Exerice 3 *)

let paire s =
  let rec aux s1 s2 =
    if Stack.length s = 0 then s1, s2 else
    match ~< s with
    | x when x mod 2 = 0 -> x >- s1; aux s1 s2
    | x -> x >- s2; aux s1 s2
  in
  aux (Stack.create ()) (Stack.create ())


(* Exerice 4 *)

let alterne s =
  let a, b = paire s in
  let rec aux s =
    if Stack.length a = 0 then begin Stack.iter (fun x -> x >- s) b; s end else
    if Stack.length b = 0 then begin Stack.iter (fun x -> x >- s) a; s end else
    match ~< a, ~< b with
    | x, y -> x >- s; y >- s; aux s
  in
  aux (Stack.create ())


(* Exercice 5*)

let mur c d = c land d <> 0

let solve x y =
  let s = Stack.create () and lab = Array.make 256 (-1) and c = ref 0 in x >- s;
  while Stack.length s <> 0 do
    let n = ~< s in
    lab.(n) <- !c; c := !c + 1;
    if n = y then Stack.clear s;
    List.iter (fun m -> let w = match m with | 1 -> -16 | 2 -> 1 | 4 -> 16 | 8 -> -1 | _ -> 0 in if lab.(n+w) = -1 then n+w >- s) (List.filter (fun m -> not(mur laby.(n) m)) [1; 2; 4; 8])
  done;
  lab


let () = Random.self_init ()
let () = Array.iter (Printf.printf "%d ") (solve 0 2)