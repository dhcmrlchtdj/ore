open Token
open Ast

(**
 * alternation = concatenation '|' alternation
 *             | concatenation
 *             | Epsilon
 * concatenation = repeation '^' concatenation
 *               | repeation
 * repeation = atom '*'
 *           | atom '+'
 *           | atom '?'
 *           | atom
 * atom = '(' alternation ')'
 *      | [a-zA-Z0-9]
*)

let recursive_descent (input:string) : re =
    let rec parse_alternation (ts:token list) : (re * (token list)) =
        let (r1, t1) = (match ts with
            | [] -> (Epsilon, [])
            | Alter::_ -> (Epsilon, ts)
            | _ -> parse_concatenation ts
        ) in
        (match t1 with
            | Alter::t ->
                let (r2, ts2) = parse_alternation t in
                let r3 = Alternation (r1, r2) in
                (r3, ts2)
            | _ -> (r1, t1))

    and parse_concatenation (ts:token list) : (re * (token list)) =
        let (r1, ts1) = parse_repeation ts in
        match ts1 with
            | Concat::t ->
                let (r2, ts2) = parse_concatenation t in
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
            | (Ch c)::t -> (Character c, t)
            | LeftParen::t ->
                let (r1, ts1) = parse_alternation t in
                (match ts1 with
                    | RightParen::t -> (r1, t)
                    | h::_ -> failwith ("atom: should ) but " ^ (Token.to_string h))
                    | [] -> failwith "atom: should ) but END")
            | RightParen::_ ->
                (Epsilon, ts)
            | _ ->
                failwith "atom: should never happen"

    in
    match parse_alternation (Lexer.scan input) with
        | r, [] -> r
        | _ -> failwith "recursive_descent unexpected"


let shunting_yard (input:string) : re =
    let rec consume_input (input:token list) (operators:token list) (operands:re list) : re list =
        match input, operators with
            | [], [] ->
                operands
            | [], _ ->
                consume_operator input operators operands

            | (Ch c)::t, _ ->
                consume_input t operators ((Character c)::operands)

            | LeftParen::t, _ ->
                consume_input t (LeftParen::operators) operands
            | RightParen::t, LeftParen::tt ->
                consume_input t tt operands
            | RightParen::_, _ ->
                consume_operator input operators operands

            | h::t, [] ->
                consume_input t (h::operators) operands
            | h::t, hh::_ when (precedence h) >= (precedence hh) ->
                consume_input t (h::operators) operands
            | _::_, _::_ ->
                consume_operator input operators operands

    and consume_operator (ts:token list) (operators:token list) (operands:re list) : re list =
        match operators, operands with
            | Alter::t, [] ->
                consume_input ts t ((Alternation (Epsilon, Epsilon)) :: [])
            | Alter::t, x1::[] ->
                consume_input ts t ((Alternation (Epsilon, x1)) :: [])
            | Alter::t, x1::x2::rands ->
                consume_input ts t ((Alternation (x2, x1)) :: rands)
            | Concat::t, x1::x2::rands ->
                consume_input ts t ((Concatenation (x2, x1)) :: rands)
            | Star::t, x1::rands ->
                consume_input ts t ((RepeationStar x1) :: rands)
            | Plus::t, x1::rands ->
                consume_input ts t ((RepeationPlus x1) :: rands)
            | Question::t, x1::rands ->
                consume_input ts t ((RepeationQuestion x1) :: rands)
            | h::_, _ ->
                failwith ("operator: should not happen, " ^ (Token.to_string h))
            | [], _ ->
                failwith "operator: should not happen, empty"

    in
    match consume_input (Lexer.scan input) [] [] with
        | [] -> Epsilon
        | [r] -> r
        | _ -> failwith "shunting_yard unexpected"


let precedence_climbing (input:string) : re =
    let rec parse_expr (ts:token list) (prev:re option) (prec:int) : (re * (token list)) =
        let (r1, t1) = (match prev with
            | None -> parse_atom ts
            | Some r -> (r, ts)
        )
        in
        match t1 with
            | [] -> r1, []
            | h::t when (precedence h) >= prec ->
                (match h with
                    | Concat | Alter -> (
                            let r2, t2 = parse_expr t None (precedence h) in
                            let r3 = infix h r1 r2 in
                            parse_expr t2 (Some r3) prec
                        )
                    | Star | Plus | Question -> (
                            let r2 = postfix h r1 in
                            parse_expr t (Some r2) prec
                        )
                    | _ -> failwith "err")
            | _::_ ->
                r1, t1

    and parse_atom (ts:token list) : (re * (token list)) =
        match ts with
            | [] -> (Epsilon, [])
            | (Ch c)::t -> (Character c, t)
            | LeftParen::t ->
                let (r1, ts1) = parse_expr t None 0 in
                (match ts1 with
                    | RightParen::t -> (r1, t)
                    | h::_ -> failwith ("precedence_climbing: atom: should ) but " ^ (Token.to_string h))
                    | [] -> failwith "precedence_climbing: atom: should ) but END")
            | RightParen::_ -> (Epsilon, ts)
            | Alter::_ -> (Epsilon, ts)
            | _ ->
                failwith "precedence_climbing: atom: should never happen"

    and infix token left right =
        match token with
            | Concat -> Concatenation (left, right)
            | Alter -> Alternation (left, right)
            | _ -> failwith "never"

    and postfix token exp =
        match token with
            | Star -> RepeationStar exp
            | Plus -> RepeationPlus exp
            | Question -> RepeationQuestion exp
            | _ -> failwith "never"

    in
    match parse_expr (Lexer.scan input) None 0 with
        | r, [] -> r
        | _ -> failwith "precedence_climbing unexpected"


let pratt (input:string) : re =
    Epsilon
