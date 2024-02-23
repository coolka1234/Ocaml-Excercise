module Quadratic = struct
  let calc a b c =
    let delta = b *. b -. 4. *. a *. c in
    match a with 
    |0.-> failwith "Nie rownanie kwadratowe" 
    |_-> match delta with 
      | a when a < 0. -> []
      | 0. -> [-.b /. (2. *. a)] 
      | _ -> let delta2 = sqrt delta in [(-.b -. delta2) /. (2. *. a); (-.b +. delta2) /. (2. *. a)]
end
module ListOperations = struct 
  let empty = []
  let is_empty lista = match lista with
    | [] -> true 
    | _ -> false
  let length lista =
    let rec lengthTail acc = function
      | [] -> acc
      | _::tail -> lengthTail (acc + 1) tail
    in
    lengthTail 0 lista
  let append x y = x @ y
  let map func lista =
    let rec mapTail acc = function
      | [] -> List.rev acc
      | head::tail -> mapTail (func head :: acc) tail
    in
    mapTail [] lista 
  let rec sum lista = match lista with
    | [] -> 0
    | head::tail -> head + sum tail 
  let rec alternate_merge x y = 
    match (x, y) with
    | (headOfX::tailOfX, headOfY::tailOfY) -> headOfX::headOfY::alternate_merge tailOfX tailOfY
    | (_, headOfY::tailOfY) -> y 
    | (headOfX::tailOfX, _) -> x 
    | (_, _) -> []
end

module TupleOperations = struct 
  let get_first tupleElem = let (x, _) = tupleElem in x
  let get_second tupleElem = let (_, y) = tupleElem in y
  let map func tupleElem = let (x, y) = tupleElem in (func x, func y)
  let add tup1 tup2 = 
    let (x1, y1) = tup1 in 
    let (x2, y2) = tup2 in 
    (x1 + x2, y1 + y2)
  let subtract tup1 tup2 = 
    let (x1, y1) = tup1 in 
    let (x2, y2) = tup2 in 
    (x1 - x2, y1 - y2)
  let combine x y = (x, y)
  let create x y = (x, y)
end 

open Quadratic;;
open ListOperations;;
open TupleOperations;;

(calc 1.5 0.8 (-.689.));; 
(calc (-0.1) 12.7 9.3);;
(calc 0. 13. (-60.));; 
(calc 1. 4. 4.);; 

(is_empty empty);;
(is_empty [8;9]);;
(length [1;1;1;1;1;1]);;
(append [1;2;4] [4; 5]);;
(ListOperations.map (fun a -> a+a) [1;8;64]);;
(sum [5;6;7;8;9]);;
(alternate_merge [1;3;5;7;9;11;13] [2;4;6;8;10]);; 

(create 0 3);;
(get_first (0, 3));;
(get_second (0, 3));;
(TupleOperations.map (fun a -> a+a) (5, 15));;
(add (1,10) (2, 30));; 
(combine (86,43) (-30, -40));;
(subtract (56,70) (678, 0));;

