type pol = Poly of int * int * int * int * int * int * int * int

let to_int pl =
  match pl with
  | Poly(x1,x2,x3,x4,x5,x6,x7,x8) -> x1 lsl 7 + x2 lsl 6 + x3 lsl 5 + x4 lsl 4 + x5 lsl 3 + x6 lsl 2 + x7 lsl 1 + x8
let (+~) a b = a + b mod 2
let (-~) a b = a - b mod 2
let (+:) a b =
  match a, b with
  | Poly(x1,x2,x3,x4,x5,x6,x7,x8), Poly(y1,y2,y3,y4,y5,y6,y7,y8) -> Poly(x1+~y1,x2+~y2,x3+~y3,x4+~y4,x5+~y5,x6+~y6,x7+~y7,x8+~y8)
let (-:) a b =
  match a, b with
  | Poly(x1,x2,x3,x4,x5,x6,x7,x8), Poly(y1,y2,y3,y4,y5,y6,y7,y8) -> Poly(x1-~y1,x2-~y2,x3-~y3,x4-~y4,x5-~y5,x6-~y6,x7-~y7,x8-~y8)



class gfArray = object (_)
  val mutable values = [|Poly(0,0,0,0,0,0,0,0); Poly(0,0,0,0,0,0,0,0); Poly(0,0,0,0,0,0,0,0); Poly(0,0,0,0,0,0,0,0);
                         Poly(0,0,0,0,0,0,0,0); Poly(0,0,0,0,0,0,0,0); Poly(0,0,0,0,0,0,0,0); Poly(0,0,0,0,0,0,0,0);
                         Poly(0,0,0,0,0,0,0,0); Poly(0,0,0,0,0,0,0,0); Poly(0,0,0,0,0,0,0,0); Poly(0,0,0,0,0,0,0,0);
                         Poly(0,0,0,0,0,0,0,0); Poly(0,0,0,0,0,0,0,0); Poly(0,0,0,0,0,0,0,0); Poly(0,0,0,0,0,0,0,0)|]
  
  method setValues l =
    for i = 0 to 3 do
      for j = 0 to 3 do
        values.(4 * i + j) <- l.(4 * i + j)
      done
    done
  
  method print =
    for i = 0 to 3 do
      for j = 0 to 3 do
        Printf.printf "%i\n" (to_int values.(4 * i + j))
      done
    done
end



let test = new gfArray
let () = test#setValues [|Poly(0,0,0,0,0,0,0,0); Poly(0,0,0,0,0,1,0,0); Poly(0,0,0,0,1,0,0,0); Poly(0,0,0,0,1,1,0,0);
                          Poly(0,0,0,0,0,0,0,1); Poly(0,0,0,0,0,1,0,1); Poly(0,0,0,0,1,0,0,1); Poly(0,0,0,0,1,1,0,1);
                          Poly(0,0,0,0,0,0,1,0); Poly(0,0,0,0,0,1,1,0); Poly(0,0,0,0,1,0,1,0); Poly(0,0,0,0,1,1,1,0);
                          Poly(0,0,0,0,0,0,1,1); Poly(0,0,0,0,0,1,1,1); Poly(0,0,0,0,1,0,1,1); Poly(0,0,0,0,1,1,1,1)|]
let () = test#print