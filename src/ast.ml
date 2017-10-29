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

let rec simplify = function
    | Concatenation (r1, r2) -> (
            match simplify r1, simplify r2 with
                | RepeationStar x, (Concatenation (RepeationStar y, _) as z)
                    when x = y -> z

                | RepeationPlus x, Concatenation (RepeationPlus y, z)
                | RepeationPlus x, Concatenation (RepeationStar y, z)
                | RepeationPlus x, Concatenation (RepeationQuestion y, z)
                | RepeationStar x, Concatenation (RepeationPlus y, z)
                | RepeationQuestion x, Concatenation (RepeationPlus y, z)
                    when x = y -> Concatenation (RepeationPlus x, z)

                | RepeationStar x, RepeationStar y
                    when x = y -> RepeationStar x

                | RepeationPlus x, RepeationPlus y
                | RepeationPlus x, RepeationQuestion y
                | RepeationPlus x, RepeationStar y
                | RepeationStar x, RepeationPlus y
                | RepeationQuestion x, RepeationPlus y
                    when x = y -> RepeationPlus x

                | RepeationPlus x, y
                | RepeationStar x, y
                    when x = y -> RepeationPlus y
                | y, RepeationPlus x
                | y, RepeationStar x
                    when x = y -> RepeationPlus y

                | Epsilon, Epsilon -> Epsilon
                | Epsilon, r | r, Epsilon -> r
                | rr1, rr2 -> Concatenation (rr1, rr2)
        )
    | Alternation (r1, r2) -> (
            match simplify r1,  simplify r2 with
                | Epsilon, Epsilon -> Epsilon

                | RepeationQuestion x, RepeationQuestion y when x = y ->
                    RepeationQuestion x

                | RepeationPlus x, RepeationPlus y when x = y ->
                    RepeationPlus x

                | RepeationQuestion x, RepeationPlus y
                | RepeationPlus x, RepeationQuestion y
                | RepeationQuestion x, RepeationStar y
                | RepeationPlus x, RepeationStar y
                | RepeationStar x, RepeationQuestion y
                | RepeationStar x, RepeationPlus y
                | RepeationStar x, RepeationStar y when x = y ->
                    RepeationStar x

                | Epsilon, RepeationPlus r
                | RepeationPlus r, Epsilon
                | Epsilon, RepeationStar r
                | RepeationStar r, Epsilon ->
                    RepeationStar r

                | Epsilon, RepeationQuestion r
                | RepeationQuestion r, Epsilon
                | Epsilon, r
                | r, Epsilon ->
                    RepeationQuestion r

                | rr1, rr2 -> Alternation (rr1, rr2)
        )
    | RepeationStar r -> (
            match simplify r with
                | Epsilon -> Epsilon
                | RepeationQuestion rr
                | RepeationPlus rr
                | RepeationStar rr
                | rr -> RepeationStar rr
        )
    | RepeationPlus r -> (
            match simplify r with
                | Epsilon -> Epsilon
                | RepeationQuestion rr | RepeationStar rr -> RepeationStar rr
                | RepeationPlus rr | rr -> RepeationPlus rr
        )
    | RepeationQuestion r -> (
            match simplify r with
                | Epsilon -> Epsilon
                | RepeationPlus rr | RepeationStar rr -> RepeationStar rr
                | RepeationQuestion rr | rr -> RepeationQuestion rr
        )
    | Character c -> Character c
    | Epsilon -> Epsilon
