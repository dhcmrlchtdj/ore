type re =
    | Concatenation of re * re
    | Alternation of re * re
    | Repeation of re
    | Character of char
    | Epsilon

let rec to_string = function
    | Concatenation (r1, r2) -> Printf.sprintf "%s%s" (to_string r1) (to_string r2)
    | Alternation (r1, r2) -> Printf.sprintf "(%s|%s)" (to_string r1) (to_string r2)
    | Repeation r -> Printf.sprintf "(%s)*" (to_string r)
    | Character c -> Char.escaped c
    | Epsilon -> ""

let print r = print_endline (to_string r)
