open Token
open Ast

(**
 * alternation = concatenation '|' alternation
 *             | concatenation
 *             | Epsilon
 * concatenation = repeation '^' concatenation
 *               | repeation
 * repeation = atom '*'
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
            | Repeat::t -> (Repeation r1, t)
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
            | Repeat::t, x1::rands ->
                consume_input ts t ((Repeation x1) :: rands)
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
                    | Repeat -> (
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
            | Repeat -> Repeation exp
            | _ -> failwith "never"

    in
    match parse_expr (Lexer.scan input) None 0 with
        | r, [] -> r
        | _ -> failwith "precedence_climbing unexpected"


let pratt (input:string) : re =
    let rec parse_expr (behind:re option) (prec:int) (ts:token list) : (re * (token list)) =
        match behind with
            | None -> parse_prefix prec ts
            | Some exp -> parse_infix exp prec ts
    and parse_prefix (prec:int) (ts:token list) : (re * (token list)) =
        match ts with
            | [] -> (Epsilon, [])
            | RightParen::_ -> (Epsilon, ts)
            | Alter::_ -> parse_expr (Some (Epsilon)) prec ts

            | (Ch c)::t ->
                parse_expr (Some (Character c)) prec t
            | LeftParen::t ->
                let (exp, tt) = (
                    match parse_expr None 0 t with
                        | exp, RightParen::tt -> (exp, tt)
                        | _ -> failwith "unmatched `)`"
                ) in
                parse_expr (Some exp) prec tt

            | _ -> failwith "None: unexpected"
    and parse_infix (exp:re) (prec:int) (ts:token list) : (re * (token list)) =
        match ts with
            | [] -> (exp, [])
            | RightParen::_ -> (exp, ts)

            | h::t when is_postfix h ->
                if prec < precedence h
                then (
                    let e = build_postfix h exp in
                    parse_expr (Some e) prec t
                ) else (
                    (exp, ts)
                )

            | h::t when is_infix_right h ->
                if prec <= precedence h
                then (
                    let (e2, tt) = parse_expr None (precedence h) t in
                    let e = build_infix h exp e2 in
                    parse_expr (Some e) prec tt
                ) else (
                    (exp, ts)
                )

            | _ -> failwith "Some: unexpected"
    and is_postfix = function
        | Repeat -> true
        | _ -> false
    and is_infix_right = function
        | Alter | Concat -> true
        | _ -> false
    and build_postfix op exp =
        match op with
            | Repeat -> Repeation exp
            | _ -> failwith "never"
    and build_infix op left right =
        match op with
            | Alter -> Alternation (left, right)
            | Concat -> Concatenation (left, right)
            | _ -> failwith "never"
    in
    match parse_expr None 0 (Lexer.scan input) with
        | r, [] -> r
        | _ -> failwith "pratt unexpected"
