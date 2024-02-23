type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

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

let rec llist_to_list ll =
  match ll with
  | LNil -> []
  | LCons (x, xf) -> x :: llist_to_list (xf ());;

let rec lpodziel lista =
  let rec lparzyste lista =
    match lista with
    | LCons (x, xf) -> LCons (x, fun () -> lnparzyste (xf ()))
    | LNil -> LNil
  and lnparzyste lista =
    match lista with
    | LCons (x, xf) -> lparzyste (xf ())
    | LNil -> LNil
  in
  LCons (lparzyste lista, fun () -> LCons (lnparzyste lista, fun () -> LNil))
;;
let podziel lista =
  let rec podziel_pom lista even_list odd_list =
    match lista with
    | [] -> (List.rev even_list, List.rev odd_list)
    | x :: xs ->
        if x mod 2 = 0 then
          podziel_pom xs (x :: even_list) odd_list
        else
          podziel_pom xs even_list (x :: odd_list)
  in
  podziel_pom lista [] []
;;
ltake(10,lhd(lpodziel(lfrom(10))));;
ltake(10,lhd(ltl(lpodziel(lfrom(10)))));;
ltake(10,lhd(lpodziel(lfrom(34))));;
ltake(10,lhd(ltl(lpodziel(lfrom(34)))));;
ltake(10,lhd(lpodziel(lfrom(2))));;
ltake(10,lhd(ltl(lpodziel(lfrom(2)))));;
podziel([1;3;5;7;9;11;12]);;
podziel([1;3;5;7;9;11]);;
podziel([2;4;5;8]);;
podziel([1;3;5;7;9;11]);;
