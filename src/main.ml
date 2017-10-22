type token =
    | Alternation
    | Star
    | Plus
    | Question
    | LeftParen
    | RightParen
    | Char of char
    | LeftB
    | RightB
    | Dash
    | Dot

let lexer (re:string) : token list =
    let rec aux acc s =
        match Stream.peek s with
            | Some '|' -> Stream.junk s; aux (Alternation :: acc) s
            | Some '*' -> Stream.junk s; aux (Star :: acc) s
            | Some '+' -> Stream.junk s; aux (Plus :: acc) s
            | Some '?' -> Stream.junk s; aux (Question :: acc) s
            | Some '(' -> Stream.junk s; aux (LeftParen :: acc) s
            | Some ')' -> Stream.junk s; aux (RightParen :: acc) s
            | Some '\\' -> Stream.junk s;
                (match Stream.peek s with
                    | Some c -> Stream.junk s; aux (Char c :: acc) s
                    | None -> failwith "require char")
            | Some c -> Stream.junk s; aux (Char c :: acc) s
            | None -> List.rev acc
    in
    aux [] (Stream.of_string re)

let to_NFA () = ()
let simulate_NFA () = ()
let to_DFA () = ()

type state = {
    c: char;
    out: state list;
}
and frag = {
    start: state;
    out1: int list;
}
