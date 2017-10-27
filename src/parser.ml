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

let parse_rd (input:string) : re =
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
    parse_alternation (S.of_string input)


type token =
    | Concat
    | Alter
    | Repeat
    | Letter of char
    | Open
    | Close
let to_tokens (input:string) : token list =
    let rec aux cs ts =
        match S.peek cs with
            | None -> List.rev ts
            | Some c -> (
                    S.junk cs;
                    match c with
                        | '(' -> aux cs (Open::ts)
                        | ')' -> aux cs (Close::ts)
                        | '*' -> aux cs (Repeat::ts)
                        | '|' -> aux cs (Alter::ts)
                        | _ -> aux cs ((Letter c)::ts)
                )
    in
    aux (S.of_string input) []

let parse_shunting_yard (input:string) : re =
    let precedence = function
        | Open -> 0
        | Close -> 0
        | Letter _ -> 0
        | Repeat -> 3
        | Concat -> 2
        | Alter -> 1
    in
    let rec consume_token (s:token list) (operators:token list) (operands:re list) (need_concat:bool) : re =
        match s, operators with
            | [], [] ->
                List.hd operands
            | [], _ ->
                consume_operator s operators operands

            | (Letter c)::t, _ ->
                if need_concat
                then consume_token (Concat::s) operators operands false
                else consume_token t operators ((Letter c)::operands) true

            | Open::t, _ ->
                if need_concat
                then consume_token (Concat::s) operators operands false
                else consume_token t (Open::operators) operands false

            | Close::t, Open::tt ->
                consume_token t tt operands true
            | Close::_, _ ->
                consume_operator s operators operands

            | h::t, [] ->
                consume_token t (h::operators) operands false
            | h::t, hh::_ when (precedence h) >= (precedence hh) ->
                consume_token t (h::operators) operands false
            | _::_, _::_ ->
                consume_operator s operators operands
    and consume_operator s operators operands =
        match operators, operands with
            | Alter::t, x1::x2::rands ->
                consume_token s t ((Alternation (x2, x1)) :: rands) false
            | Concat::t, x1::x2::rands ->
                consume_token s t ((Concatenation (x2, x1)) :: rands) false
            | Repeat::t, x::rands ->
                consume_token s t ((Repeation x) :: rands) false
            | _ -> failwith ""
    in
    consume_token (to_tokens input) [] [] false

let _ =
    (* parse_rd ""            |> print; *)
    (* parse_rd "a"           |> print; *)
    (* parse_rd "abc"         |> print; *)
    (* parse_rd "a|b"         |> print; *)
    (* parse_rd "a|b|c"       |> print; *)
    (* parse_rd "a|"          |> print; *)
    (* parse_rd "|b"          |> print; *)
    (* parse_rd "|"           |> print; *)
    (* parse_rd "||"          |> print; *)
    (* parse_rd "(ab)c"       |> print; *)
    (* parse_rd "a(bc)"       |> print; *)
    (* parse_rd "(a|)b"       |> print; *)
    (* parse_rd "(|a)b"       |> print; *)
    (* parse_rd "(a||)b"      |> print; *)
    ()
