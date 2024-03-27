let pow_res = Learn_ocaml.L2.pow 8 2

let () = print_endline @@ string_of_int pow_res

let valid = Learn_ocaml.L2.valid_date "Mar" 28

let () = if valid then print_endline "Date is valid" else print_endline "Date is invalid"
