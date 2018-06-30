open Batteries

type token = Concat | Alter | Repeat | Ch of char | LeftParen | RightParen

let to_string = function
    | Concat -> "^"
    | Alter -> "|"
    | Repeat -> "*"
    | Ch c -> String.of_char c
    | LeftParen -> "("
    | RightParen -> ")"

let precedence = function
    | Ch _ -> -10
    | LeftParen -> -10
    | RightParen -> -10
    | Repeat -> 30
    | Concat -> 20
    | Alter -> 10
