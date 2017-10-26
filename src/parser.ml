module S = Stream

type re =
    | Concatenation of re * re
    | Alternation of re * re
    | Repeation of re
    | Letter of char
    | Epsilon

let rec to_string = function
    | Concatenation (r1, r2) -> Printf.sprintf "%s%s" (to_string r1) (to_string r2)
    | Alternation (r1, r2) -> Printf.sprintf "(%s | %s)" (to_string r1) (to_string r2)
    | Repeation r -> Printf.sprintf "(%s)*" (to_string r)
    | Letter c -> Char.escaped c
    | Epsilon -> ""
let print r = print_endline (to_string r)

let parse (input:string) : re =
    let rec parse_alternation s : re =
        let r = parse_concatenation s in
        match S.peek s with
            | None -> r
            | Some ')' ->
                S.junk s;
                r
            | Some '|' ->
                S.junk s;
                let r2 = parse_alternation s in
                Alternation (r, r2)
            | Some _ -> failwith "concat"
    and parse_concatenation s : re =
        let r = parse_repeation s in
        match S.peek s with
            | None -> r
            | Some '|' | Some ')' -> r
            | Some _ ->
                let r2 = parse_concatenation s in
                Concatenation (r, r2)
    and parse_repeation s : re =
        let r = parse_atom s in
        match S.peek s with
            | None -> r
            | Some '*' ->
                S.junk s;
                Repeation r
            | Some _ -> r
    and parse_atom s : re =
        match S.peek s with
            | None -> Epsilon
            | Some '(' ->
                S.junk s;
                parse_alternation s
            | Some c ->
                S.junk s;
                Letter c
    in
    parse_alternation (Stream.of_string input)

let _ =
    parse ""            |> print;
parse "a"           |> print;
parse "abc"         |> print;
parse "a|b"         |> print;
parse "a|b|c"       |> print;
parse "a|"          |> print;
parse "|b"          |> print;
parse "|"           |> print;
parse "||"          |> print;
parse "(ab)c"       |> print;
parse "a(bc)"       |> print;
parse "(a|)b"       |> print;
parse "(|a)b"       |> print;
parse "(a||)b"      |> print;
