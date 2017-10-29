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

(* all right associativity *)
let is_infix = function
    | Concat | Alter -> true
    | _ -> false

let precedence = function
    | Ch _ -> -10
    | LeftParen -> -10
    | RightParen -> -10
    | Star -> 30
    | Plus -> 30
    | Question -> 30
    | Concat -> 20
    | Alter -> 10

let to_string = function
    | Concat -> "^"
    | Alter -> "|"
    | Star -> "*"
    | Plus -> "+"
    | Question -> "?"
    | Ch c -> Char.escaped c
    | LeftParen -> "("
    | RightParen -> ")"
