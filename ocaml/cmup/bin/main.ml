type name = A | B | C

let get_name x = match x with 
                 | A -> "Victor"
                 | B -> "Yenoch"
                 | C -> "Kuptsov"

let () = print_endline (get_name A);
         print_endline (get_name B);
         print_endline (get_name C)
