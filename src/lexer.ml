open Token

let scan (input:string) : token list =
    let rec aux cs ts =
        match Stream.peek cs with
            | None -> List.rev ts
            | Some c -> (
                    Stream.junk cs;
                    match c with
                        | '(' -> aux cs (Open::ts)
                        | ')' -> aux cs (Close::ts)
                        | '*' -> aux cs (Repeat::ts)
                        | '|' -> aux cs (Alter::ts)
                        | _ -> aux cs ((Ch c)::ts)
                )
    in
    aux (Stream.of_string input) []
