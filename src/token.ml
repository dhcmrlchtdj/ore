type token =
    | Concat
    | Alter
    | Star
    | Plus
    | Question
    | Ch of char
    | LeftParen
    | RightParen

let to_string = function
    | Concat -> "^"
    | Alter -> "|"
    | Star -> "*"
    | Plus -> "+"
    | Question -> "?"
    | Ch c -> Char.escaped c
    | LeftParen -> "("
    | RightParen -> ")"

let precedence = function
    | Ch _ -> -10
    | LeftParen -> -10
    | RightParen -> -10
    | Star -> 30
    | Plus -> 30
    | Question -> 30
    | Concat -> 20
    | Alter -> 10
