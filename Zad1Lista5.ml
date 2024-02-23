type code = Elem of bool | Not | And | Or | Xor;;

let eval le =
  List.hd (List.fold_left (fun stack e -> (* List.hd bo wynikiem jest ostatnie miejsce na stosie *)
      (match e with
       | Elem(b) -> 
           b::stack
       | Not -> 
           if List.length stack < 1 then failwith "Za malo argumentow dla not"
           else (not (List.hd stack))::(List.tl stack)
       | And -> 
           if List.length stack < 2 then failwith "Za malo argumentow dla and"
           else ((List.hd stack) && (List.hd (List.tl stack)))::(List.tl (List.tl stack))
       | Or -> 
           if List.length stack < 2 then failwith "Za malo argumentow dla or"
           else ((List.hd stack) || (List.hd (List.tl stack)))::(List.tl (List.tl stack))
       | Xor -> 
           if List.length stack < 2 then failwith "Za malo argumentow dla xor"
           else ((List.hd stack) || (List.hd (List.tl stack)) || (not ((List.hd stack) && (List.hd (List.tl stack))))) ::(List.tl (List.tl stack)) 
      ))[] le);; (*Pusta lista "[]" to nic innego jak akumulator, zeby fold_left bylo ogonowe *)

eval [Elem true];;
eval [Elem true; Not];;
eval [Elem true; Not; Elem false; Or; Elem true; Xor; Elem true; And];;
eval [Elem true;And];;
eval [Elem false; Elem true; Not; Elem false; Or; Elem true; Xor; Elem true; And];;
  