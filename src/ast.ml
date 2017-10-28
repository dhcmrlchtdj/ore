type re =
    | Concatenation of re * re
    | Alternation of re * re
    | RepeationStar of re
    | RepeationPlus of re
    | RepeationQuestion of re
    | Character of char
    | Epsilon

let rec to_string = function
    | Concatenation (r1, r2) -> Printf.sprintf "(^ %s %s)" (to_string r1) (to_string r2)
    | Alternation (r1, r2) -> Printf.sprintf "(| %s %s)" (to_string r1) (to_string r2)
    | RepeationStar r -> Printf.sprintf "(* %s)" (to_string r)
    | RepeationPlus r -> Printf.sprintf "(+ %s)" (to_string r)
    | RepeationQuestion r -> Printf.sprintf "(? %s)" (to_string r)
    | Character c -> Printf.sprintf "%s" (Char.escaped c)
    | Epsilon -> "_"

let print r = print_endline (to_string r)

let rec simplify = function
    | Concatenation (r1, r2) -> (
            match simplify r1,  simplify r2 with
                | Epsilon, Epsilon -> Epsilon
                | Epsilon, r | r, Epsilon -> r
                | rr1, rr2 -> Concatenation (rr1, rr2)
        )
    | Alternation (r1, r2) -> (
            match simplify r1,  simplify r2 with
                | Epsilon, Epsilon -> Epsilon
                | Epsilon, RepeationQuestion r
                | RepeationQuestion r, Epsilon
                | Epsilon, r | r, Epsilon -> RepeationQuestion r
                | rr1, rr2 -> Alternation (rr1, rr2)
        )
    | RepeationStar r -> (
            match simplify r with
                | Epsilon -> Epsilon
                | RepeationStar rr | rr -> RepeationStar rr
        )
    | RepeationPlus r -> (
            match simplify r with
                | Epsilon -> Epsilon
                | RepeationPlus rr | rr -> RepeationPlus rr
        )
    | RepeationQuestion r -> (
            match simplify r with
                | Epsilon -> Epsilon
                | RepeationQuestion rr | rr -> RepeationQuestion rr
        )
    | Character c -> Character c
    | Epsilon -> Epsilon
