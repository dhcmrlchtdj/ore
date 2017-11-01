type re =
    | Epsilon
    | Character of char
    | Repeation of re
    | Concatenation of re * re
    | Alternation of re * re

let rec to_string = function
    | Epsilon -> "_"
    | Character c ->
        Printf.sprintf "%s" (Char.escaped c)
    | Repeation r ->
        Printf.sprintf "(* %s)" (to_string r)
    | Concatenation (r1, r2) ->
        Printf.sprintf "(^ %s %s)" (to_string r1) (to_string r2)
    | Alternation (r1, r2) ->
        Printf.sprintf "(| %s %s)" (to_string r1) (to_string r2)

let rec simplify = function
    | Epsilon -> Epsilon
    | Character c -> Character c
    | Repeation r -> (
            match simplify r with
                | Epsilon -> Epsilon
                | Repeation rr | rr -> Repeation rr
        )
    | Concatenation (r1, r2) -> (
            match simplify r1, simplify r2 with
                | Repeation x, Concatenation (Repeation y, z) when x = y -> Concatenation (Repeation x, z)
                | Repeation x, Concatenation (y, z) when x = y -> Concatenation (Repeation x, z)
                | x, Concatenation (Repeation y, z) when x = y -> Concatenation (Repeation x, z)
                | Repeation x, Repeation y when x = y -> Repeation x
                | x, Repeation y when x = y -> Repeation x
                | Repeation x, y when x = y -> Repeation x
                | Epsilon, Epsilon -> Epsilon
                | Epsilon, r | r, Epsilon -> r
                | rr1, rr2 -> Concatenation (rr1, rr2)
        )
    | Alternation (r1, r2) -> (
            match simplify r1,  simplify r2 with
                | Epsilon, Epsilon -> Epsilon
                | Epsilon, Repeation r | Repeation r, Epsilon -> Repeation r
                | Repeation x, Repeation y when x = y -> Repeation x
                | x, Repeation y when x = y -> Repeation x
                | Repeation x, y when x = y -> Repeation x
                | rr1, rr2 -> Alternation (rr1, rr2)
        )
