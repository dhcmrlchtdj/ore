open Token

let explode s =
    let rec exp i l =
        if i < 0
        then l
        else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []

let scan (input:string) : token list =
    let rec aux (cs:char list) (ts:token list) : token list =
        match cs with
            | [] -> List.rev ts
            | '(' :: t -> aux t (LeftParen :: ts)
            | ')' :: t -> aux t (RightParen :: ts)
            | '*' :: t -> aux t (Star :: ts)
            | '+' :: t -> aux t (Plus :: ts)
            | '?' :: t -> aux t (Question :: ts)
            | '|' :: t -> aux t (Alter :: ts)
            | c :: t -> aux t ((Ch c) :: ts)
    in
    aux (explode input) []
