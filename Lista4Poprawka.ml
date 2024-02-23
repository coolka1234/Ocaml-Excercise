let abs lt =
  List.map (fun (x,y,z) -> (abs x, abs y, abs z)) lt;;
let rec contains lst value =
  match lst with
  | [] -> false
  | x :: xs -> x = value || contains xs value
;;
let rec filter lists value =
  let rec filterHelp restOfList acc =
    match restOfList with
    | [] -> acc
    | head :: tail ->
        if contains head value then
          filterHelp tail (acc @ [head])
        else
          filterHelp tail acc
  in
  filterHelp lists []
;;
let rec checkIfBin li =
  match li with
  | [] -> true
  | hd :: tl ->
      if hd > 1 || hd < 0 then false
      else checkIfBin tl ;;
let bin2dec lb =
  if(checkIfBin lb) then List.fold_left (fun acc b -> acc*2 + b) 0 lb
  else raise (Failure "Liczby nie binarne");; 
filter [[1;2;3];[3;3];[5;6];[-1;7]] 6;;
filter [] 10;;
filter [[1;2;3];[3;8];[5;6];[-1;8]] 8;;
bin2dec [1;0;-1;0];;
bin2dec [];;
bin2dec [1;1;1;1;1;1;1;1;0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;0;1;1;1;1];;
abs [(-1,-2,-3);(-1,2,4)];;
abs [];;
abs [(-12,-9,-1);(-6,5,-4)];;
  