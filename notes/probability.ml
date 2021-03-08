open Random

let coin_flip () =
  let rec loop x c = 
    if x = 0 
      then loop (Random.int 2) (c+1)
      else print_endline ("Terminated in " ^ string_of_int c ^ " steps."); 
  in
  loop 0 0