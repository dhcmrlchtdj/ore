open Batteries
open Token

let right_allow_concat = function
    | Concat -> false
    | Alter -> false
    | Repeat -> true
    | Ch _ -> true
    | LeftParen -> false
    | RightParen -> true

and left_allow_concat = function
    | Concat -> false
    | Alter -> false
    | Repeat -> false
    | Ch _ -> true
    | LeftParen -> true
    | RightParen -> false

let rec insert_concat (tokens: token list) : token list =
    match tokens with
        | [] | [_] -> tokens
        | x :: y :: z when right_allow_concat x && left_allow_concat y ->
            x :: Concat :: insert_concat (y :: z)
        | h :: t -> h :: insert_concat t

let scan (input: string) : token list =
    let rec aux (cs: char list) (ts: token list) : token list =
        match cs with
            | [] -> List.rev ts
            | ' ' :: t -> aux t ts
            | '(' :: t -> aux t (LeftParen :: ts)
            | ')' :: t -> aux t (RightParen :: ts)
            | '*' :: t -> aux t (Repeat :: ts)
            | '|' :: t -> aux t (Alter :: ts)
            | c :: t -> aux t (Ch c :: ts)
    in
    aux (String.to_list input) [] |> insert_concat
