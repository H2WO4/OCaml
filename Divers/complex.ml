type plx =
  | Complex of float * float

let complex x y = Complex(x, y)
let complex_int x y = complex (float x) (float y)
let ( ~|| ) x =
  match x with
  | Complex(a, b) -> sqrt (a ** 2. +. b ** 2.)
let ( ~:: ) x =
  match x with
  | Complex(a, b) -> atan2 b a
let print_complex x =
  match x with
  | Complex(a, 0.) -> print_string (string_of_float a)
  | Complex(0., b) -> print_string ("i" ^ string_of_float b)
  | Complex(a, b) when b < 0. -> print_string (string_of_float a ^ "-i" ^ string_of_float (-.b))
  | Complex(a, b) -> print_string (string_of_float a ^ "+i" ^ string_of_float b)
let print_complex_exp x =
  match x with
  | Complex(a, 0.) -> print_string (string_of_float a ^ "e^(0i)")
  | Complex(_, _) when ~|| x == 1. -> print_string ("e^(" ^ string_of_float ~:: x ^ "i)")
  | Complex(_, _) when ~|| x == -1. -> print_string ("-e^(" ^ string_of_float ~:: x ^ "i)")
  | Complex(_, _) -> print_string ((string_of_float ~|| x) ^ "*e^(" ^ string_of_float ~:: x ^ "i)")
let ( +: ) x y =
  match x, y with
  | Complex(a, b), Complex(c, d) -> complex (a +. c) (b +. d)
let ( -: ) x y =
  match x, y with
  | Complex(a, b), Complex(c, d) -> complex (a -. c) (b -. d)
let ( ~-: ) x =
  match x with
  | Complex(a, b) -> complex (-.a) (-.b)
let ( ~: ) x =
  match x with
  | Complex(a, b) -> complex a (-.b)
let ( *: ) x y =
  match x, y with
  | Complex(a, b), Complex(c, 0.) -> complex (a *. c) (b *. c)
  | Complex(a, 0.), Complex(c, d) -> complex (a *. c) (a *. d)
  | Complex(a, b), Complex(c, d) -> complex (a *. c -. b *. d) (a *. d +. b *. c)
let rec ( /: ) x y =
  match x, y with
  | Complex(1., 0.), Complex(c, d) -> complex (c /. (c ** 2. +. d ** 2.)) (-.d /. (c ** 2. +. d ** 2.))
  | Complex(a, b), Complex(c, 0.) -> complex (a /. c) (b /. c)
  | Complex(_, _), Complex(_, _) -> x *: (complex 1. 0. /: y)
let ( **: ) x y =
  match x, y with
  | Complex(_, _), Complex(c, d) ->
    let f = ~|| x and g = ~:: x in (complex (f ** c *. exp (-.d *. g)) 0.) *: (complex (cos (d *. (log f) +. c *. g)) (sin (d *. (log f) +. c *. g)))
let ( //: ) x y =
  match x, y with
  | Complex(_, _), Complex(_, _) -> x **: ((complex 1. 0.) /: y)


let () = print_complex (complex 3. 4. **: complex 5. 2.)  