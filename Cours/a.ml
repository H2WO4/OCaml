

let couper p = 
  let pnb,pvaleur = [],[] in
     let rec couperR p x = 
       match p with
       |[] -> pnb,pvaleur
       |h::t when x mod 2 = 0 -> pnb@[h]; couperR t (x+1)
       |h::t when x mod 2 = 1 -> pvaleur@[h]; couperR t (x+1)
       |h::[] -> pvaleur@[h]; couperR [] (x+1)
     in couperR p 0
     ;;