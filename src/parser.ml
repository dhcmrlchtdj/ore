open Token
open Ast

module T = Token

(**
 * alternation = concatenation '|' alternation
 *             | concatenation
 * concatenation  = repeation *
 * repeation = atom '*'
 *           | atom
 * atom = '(' alternation ')'
 *      | [a-zA-Z0-9]
 *      | Epsilon
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
    let rec consume_input (input:token list) (operators:token list) (operands:re list) (need_concat:bool) : re =
        match input, operators with
            | [], [] ->
                consume_operand input operators operands
            | [], _ ->
                consume_operator input operators operands

            | (Ch c)::t, _ ->
                if need_concat
                then consume_input input (Concat::operators) operands false
                else consume_input t operators ((Character c)::operands) true

            | Open::t, _ ->
                if need_concat
                then consume_input input (Concat::operators) operands false
                else consume_input t (Open::operators) operands false
            | Close::t, Open::tt ->
                consume_input t tt operands true
            | Close::_, _ ->
                consume_operator input operators operands

            | h::t, [] ->
                consume_input t (h::operators) operands (h = Repeat)
            | h::t, hh::_ when (T.precedence h) >= (T.precedence hh) ->
                consume_input t (h::operators) operands (h = Repeat)
            | _::_, _::_ ->
                consume_operator input operators operands
    and consume_operator ts operators operands =
        match operators, operands with
            | Alter::t, [] ->
                consume_input ts t ((Alternation (Epsilon, Epsilon)) :: []) false
            | Alter::t, x1::[] ->
                consume_input ts t ((Alternation (Epsilon, x1)) :: []) false
            | Alter::t, x1::x2::rands ->
                consume_input ts t ((Alternation (x2, x1)) :: rands) false
            | Concat::t, x1::x2::rands ->
                consume_input ts t ((Concatenation (x2, x1)) :: rands) false
            | Repeat::t, x1::rands ->
                consume_input ts t ((Repeation x1) :: rands) false
            | h::_, _ ->
                failwith ("operator: should not happen, " ^ (T.to_string h))
            | [], _ ->
                failwith "operator: should not happen, empty"
    and consume_operand _ _ (operands:re list) : re =
        match operands with
            | [] -> Epsilon
            | [x] -> x
            | _ ->
                print_newline ();
                (List.iter (Ast.print) operands);
                failwith "operand: should not happen"
    in
    consume_input (Lexer.scan input) [] [] false
