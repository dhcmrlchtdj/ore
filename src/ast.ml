type re =
    | Concatenation of re * re
    | Alternation of re * re
    | Repeation of re
    | Character of char
    | Epsilon

let rec to_string = function
    | Concatenation (r1, r2) -> Printf.sprintf "(^ %s %s)" (to_string r1) (to_string r2)
    | Alternation (r1, r2) -> Printf.sprintf "(| %s %s)" (to_string r1) (to_string r2)
    | Repeation r -> Printf.sprintf "(* %s)" (to_string r)
    | Character c -> Printf.sprintf "%s" (Char.escaped c)
    | Epsilon -> "_"

let rec simplify = function
    | Concatenation (r1, r2) -> (
            match simplify r1, simplify r2 with
                | Repeation x, (Concatenation (Repeation y, _) as z) when x = y -> z
                | Repeation x, Repeation y when x = y -> Repeation x
                | Epsilon, Epsilon -> Epsilon
                | Epsilon, r | r, Epsilon -> r
                | rr1, rr2 -> Concatenation (rr1, rr2)
        )
    | Alternation (r1, r2) -> (
            match simplify r1,  simplify r2 with
                | Epsilon, Epsilon -> Epsilon
                | Epsilon, Repeation r | Repeation r, Epsilon -> Repeation r
                | Repeation x, Repeation y when x = y -> Repeation x
                | rr1, rr2 -> Alternation (rr1, rr2)
        )
    | Repeation r -> (
            match simplify r with
                | Epsilon -> Epsilon
                | Repeation rr | rr -> Repeation rr
        )
    | Character c -> Character c
    | Epsilon -> Epsilon
