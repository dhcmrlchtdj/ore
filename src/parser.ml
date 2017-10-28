open Token
open Ast

module T = Token

(**
 * alternation  ::= concatenation '|' alternation
 *              ::= concatenation
 * concatenation    ::= repeation *
 * repeation    ::= atom '*'
 *              ::= atom
 * atom     ::= '(' alternation ')'
 *          ::= [a-zA-Z0-9]
 *          ::= Epsilon
*)


let recursive_descent (input:string) : re =
    let rec parse_alternation (ts:token list) : (re * (token list)) =
        let (r1, ts1) = parse_concatenation ts in
        match ts1 with
            | Alter::t ->
                let (r2, ts2) = parse_alternation t in
                let r3 = Alternation (r1, r2) in
                (r3, ts2)
            | _ -> (r1, ts1)
    and parse_concatenation (ts:token list) : (re * (token list)) =
        let (r1, ts1) = parse_repeation ts in
        match ts1 with
            | (Ch _)::_ | Open::_ ->
                let (r2, ts2) = parse_concatenation ts1 in
                (Concatenation (r1, r2), ts2)
            | _ -> (r1, ts1)
    and parse_repeation (ts:token list) : (re * (token list)) =
        let (r1, ts1) = parse_atom ts in
        match ts1 with
            | Repeat::t -> (Repeation r1, t)
            | _ -> (r1, ts1)
    and parse_atom (ts:token list) : (re * (token list)) =
        match ts with
            | [] | Alter::_ | Close::_ -> (Epsilon, ts)
            | Open::t ->
                let (r1, ts1) = parse_alternation t in
                (match ts1 with
                    | Close::t -> (r1, t)
                    | h::_ -> failwith ("atom: should ) but " ^ (T.to_string h))
                    | [] -> failwith "atom: should ) but END")
            | (Ch c)::t -> (Character c, t)
            | Concat::_ | Repeat::_ -> failwith "atom: should never happen"
    in
    match parse_alternation (Lexer.scan input) with
        | r, [] -> r
        | _ -> failwith "not end"


let shunting_yard (input:string) : re =
    let rec consume_token (s:token list) (operators:token list) (operands:re list) (need_concat:bool) : re =
        match s, operators with
            | [], [] ->
                (match operands with
                    | [] -> Epsilon
                    | [x] -> x
                    | _ -> failwith "should not happen")
            | [], _ ->
                consume_operator s operators operands

            | (Ch c)::t, _ ->
                if need_concat
                then consume_token (Concat::s) operators operands false
                else consume_token t operators ((Character c)::operands) true

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
            | h::t, hh::_ when (T.precedence h) >= (T.precedence hh) ->
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
    consume_token (Lexer.scan input) [] [] false
