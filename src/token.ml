type token =
    | Concat
    | Alter
    | Repeat
    | Ch of char
    | Open
    | Close

let precedence = function
    | Open -> 0
    | Close -> 0
    | Ch _ -> 0
    | Repeat -> 3
    | Concat -> 2
    | Alter -> 1

let to_string = function
    | Concat -> "++"
    | Alter -> "|"
    | Repeat -> "*"
    | Ch c -> Char.escaped c
    | Open -> "("
    | Close -> ")"
