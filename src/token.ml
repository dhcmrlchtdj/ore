type token =
    | Concat
    | Alter
    | Star
    | Plus
    | Question
    | Ch of char
    | LeftParen
    | RightParen

let is_postfix = function
    | Star | Plus | Question -> true
    | _ -> false
let is_infix = function
    | Concat | Alter -> true
    | _ -> false

let precedence = function
    | Ch _ -> 0
    | LeftParen -> 0
    | RightParen -> 9
    | Star -> 3
    | Plus -> 3
    | Question -> 3
    | Concat -> 2
    | Alter -> 1

let to_string = function
    | Concat -> "^"
    | Alter -> "|"
    | Star -> "*"
    | Plus -> "+"
    | Question -> "?"
    | Ch c -> Char.escaped c
    | LeftParen -> "("
    | RightParen -> ")"
