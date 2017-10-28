open Token

let scan (input:string) : token list =
    let rec aux cs ts =
        match Stream.peek cs with
            | None -> List.rev ts
            | Some c -> (
                    Stream.junk cs;
                    match c with
                        | '(' -> aux cs (LeftParen::ts)
                        | ')' -> aux cs (RightParen::ts)
                        | '*' -> aux cs (Star::ts)
                        | '+' -> aux cs (Plus::ts)
                        | '?' -> aux cs (Question::ts)
                        | '|' -> aux cs (Alter::ts)
                        | _ -> aux cs ((Ch c)::ts)
                )
    in
    aux (Stream.of_string input) []
