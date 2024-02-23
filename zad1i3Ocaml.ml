(*Ocaml zad 1*)
let rev_list list =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (hd :: acc) tl
  in
  aux [] list
;;
(*testy*)
let example_list  = [1; 2; 3; 4; 5];;
let example_list2 = [];;
let example_list3 = [5;5;5;1;5;5];;
let reversed_list = rev_list example_list;;
let reversed_list2 = rev_list example_list2;;
let reversed_list3 = rev_list example_list3;;

List.iter (fun x -> print_int x; print_string " ") reversed_list;;
List.iter (fun x -> print_int x; print_string " ") reversed_list2;;
List.iter (fun x -> print_int x; print_string " ") reversed_list3;;
(*koniec testow dla zad 1*)
(*Ocaml zad 3*)
let split_list list =
  let rec split acc_div_by_10 acc_div_by_5 acc_rest = function
    | [] -> [rev_list acc_div_by_10; rev_list acc_div_by_5; rev_list acc_rest]
    | hd :: tl ->
      match hd with
      | n when n mod 10 = 0 -> split (hd :: acc_div_by_10) acc_div_by_5 acc_rest tl
      | n when n mod 5 = 0 -> split acc_div_by_10 (hd :: acc_div_by_5) acc_rest tl
      | _ -> split acc_div_by_10 acc_div_by_5 (hd :: acc_rest) tl
  in
  split [] [] [] list
;;
(*testy*)
let example_list  = [10; 5; 20; 7; 15; 12; 3; 30];;
let example_list2 = [];;
let example_list3  = [10;20;40;1000;3;2;1;55;58;90;42;30;10];;

let result = split_list example_list;;
let result2 = split_list example_list2;;
let result3 = split_list example_list3;;
List.iter (fun sublist ->
  print_endline (String.concat "; " (List.map string_of_int sublist))
) result;;
List.iter (fun sublist ->
  print_endline (String.concat "; " (List.map string_of_int sublist))
) result2;;
List.iter (fun sublist ->
  print_endline (String.concat "; " (List.map string_of_int sublist))
) result3;;
(*koniec testow dla zad 3*)