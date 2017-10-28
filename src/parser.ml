open Token
open Ast

module T = Token

(**
 * alternation = concatenation '|' alternation
 *             | concatenation
 * concatenation  = repeation *
 * repeation = atom '*'
 *           | atom '+'
 *           | atom '?'
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
            | (Ch _)::_ | LeftParen::_ ->
                let (r2, ts2) = parse_concatenation ts1 in
                (Concatenation (r1, r2), ts2)
            | _ -> (r1, ts1)
    and parse_repeation (ts:token list) : (re * (token list)) =
        let (r1, ts1) = parse_atom ts in
        match ts1 with
            | Star::t -> (RepeationStar r1, t)
            | Plus::t -> (RepeationPlus r1, t)
            | Question::t -> (RepeationQuestion r1, t)
            | _ -> (r1, ts1)
    and parse_atom (ts:token list) : (re * (token list)) =
        match ts with
            | [] | Alter::_ | RightParen::_ -> (Epsilon, ts)
            | (Ch c)::t -> (Character c, t)
            | LeftParen::t ->
                let (r1, ts1) = parse_alternation t in
                (match ts1 with
                    | RightParen::t -> (r1, t)
                    | h::_ -> failwith ("atom: should ) but " ^ (T.to_string h))
                    | [] -> failwith "atom: should ) but END")
            | Concat::_ | Star::_ | Plus::_ | Question::_ ->
                failwith "atom: should never happen"
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
                consume_operator input operators operands false

            | (Ch c)::t, _ ->
                if need_concat
                then consume_input (Concat::input) operators operands false
                else consume_input t operators ((Character c)::operands) true

            | LeftParen::t, _ ->
                if need_concat
                then consume_input (Concat::input) operators operands false
                else consume_input t (LeftParen::operators) operands false
            | RightParen::t, LeftParen::tt ->
                consume_input t tt operands true
            | RightParen::_, _ ->
                consume_operator input operators operands false

            | h::t, [] ->
                consume_input t (h::operators) operands (is_repeation h)
            | h::t, hh::_ when (T.precedence h) >= (T.precedence hh) ->
                consume_input t (h::operators) operands (is_repeation h)
            | h::_, _::_ ->
                consume_operator input operators operands (is_repeation h)
    and consume_operator ts operators operands need_concat =
        match operators, operands with
            | Alter::t, [] ->
                consume_input ts t ((Alternation (Epsilon, Epsilon)) :: []) need_concat
            | Alter::t, x1::[] ->
                consume_input ts t ((Alternation (Epsilon, x1)) :: []) need_concat
            | Alter::t, x1::x2::rands ->
                consume_input ts t ((Alternation (x2, x1)) :: rands) need_concat
            | Concat::t, x1::x2::rands ->
                consume_input ts t ((Concatenation (x2, x1)) :: rands) need_concat
            | Star::t, x1::rands ->
                consume_input ts t ((RepeationStar x1) :: rands) need_concat
            | Plus::t, x1::rands ->
                consume_input ts t ((RepeationPlus x1) :: rands) need_concat
            | Question::t, x1::rands ->
                consume_input ts t ((RepeationQuestion x1) :: rands) need_concat
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
    and is_repeation = function
        | Star | Plus | Question -> true
        | _ -> false
    in
    consume_input (Lexer.scan input) [] [] false


(**
 * s = e_alter
 * e_alter = e_concat
 *         | e_alter "|" e_concat
 * e_concat = e_repeat
 *          | e_concat e_repeat
 * e_repeat = p "*" | p "+" | p "?"
 * p = "(" e0 ")"
 *   | [a-zA-Z0-9]
 *   | Epsilon
*)
let precedence_climbing (input:string) : re =
    let rec aux (prev_op:int) (ts:token list) : (re * (token list)) =
        match ts with
            | [] -> (Epsilon, [])

            | (Ch c)::[] -> (Character c, [])
            | (Ch c)::((Ch _)::_ as t)
            | (Ch c)::((LeftParen::_) as t) ->
                let (r1, ts1) = aux (T.precedence (Ch c)) t in
                (Concatenation (Character c, r1), ts1)

            | h::t when (T.precedence h) > prev_op ->
                let (r1, ts1) = aux (T.precedence h) t in
                (r1, ts1)
    in
    match aux (-1) (Lexer.scan input) with
        | r, [] -> r
        | _ -> failwith "not end"


(* let pratt (input:string) : re = Epsilon *)
