type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;
type 'a nlist = Koniec | Element of 'a * ('a nlist);;
let rec lfrom k = LCons (k, function () -> lfrom (k+1));;
let rec ltake = function
  | (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x,xf)) -> x::ltake(n-1, xf());;
let lhd = function
    LNil -> failwith "lhd"
  | LCons (x, _) -> x
;;
let ltl = function
    LNil -> failwith "ltl"
  | LCons (_, xf) -> xf();;

let rec lpolacz(lla, llb) =
  match (lla, llb) with
  | (LCons(a, at), LCons(b, bt)) -> LCons(a,fun()-> LCons(b,fun()->lpolacz(at(),bt())))
  | (LNil, LCons(b, bt)) -> LCons(b,fun()->bt())
  | (LCons(a, at), LNil) -> LCons(a, fun()->at())
  | _ -> LNil;;
;; 
let rec polacz(nlista, nlistb) =
  match (nlista, nlistb) with
  | (Element(a,at), Element(b,bt)) -> Element(a,polacz(nlistb, at))
  | (Koniec, Element(b,bt)) -> Element(b,bt)
  | (Element(a,at), Koniec) -> Element(a,at)
  | _ -> Koniec
;;
let rec duplicate(b, t) =
  let rec replicate(x, times, acc) =
    if times > 0 then replicate(x, times - 1, x::acc)
    else List.rev acc in
    
  match (b, t) with
  | (hb::_, []) -> []
  | (hb::tb, ht::tt) -> replicate(hb, ht, [])@duplicate(tb, tt)
  | _ -> []
;;

duplicate([1;2;3], [0;3;1;4]);;
duplicate([1;2;3], [-1;15;4]);;
duplicate([1;2;3], [5;13]);;
duplicate([1;2;3], [3;2;1]);; 
polacz(Element(1,Element(3,Element(5,Koniec))), Element(2,Element(4,Koniec)));; 
polacz(Element(1,Element(2,Element(3,Koniec))), Koniec);;
polacz(Element(1,Element(2,Element(3,Koniec))), Element(67,Element(68,Koniec)));; 
ltake(20, lpolacz(lfrom(4), lfrom(20)));;
ltake(20, lpolacz(lfrom(-1), LNil));;
ltake(20, lpolacz(LNil, lfrom(80)));;
