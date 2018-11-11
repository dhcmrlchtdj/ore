open Ast

type state = int

and edge = Eps | Ch of char

and arrow = state * edge * state

and states = arrow list

and nfa = states * state

let _end : state = 0

let to_nfa (exp : re) : nfa =
    let i = ref 0 in
    let get () : state =
        incr i;
        !i
    in
    let rec aux (acc : states) (prev : state) (exp : re) : states * state =
        match exp with
            | Epsilon ->
                let e_in = get () in
                let e_c = (e_in, Eps, prev) in
                (e_c :: acc, e_in)
            | Character c ->
                let c_in = get () in
                let c_c = (c_in, Ch c, prev) in
                (c_c :: acc, c_in)
            | Repeation r ->
                let rep_out = get () in
                let rep_in = get () in
                let sub_out = get () in
                let sub_acc, sub_in = aux acc sub_out r in
                let rep_acc =
                    (rep_in, Eps, rep_out)
                    :: (rep_in, Eps, sub_in)
                    :: (sub_out, Eps, sub_in)
                    :: (sub_out, Eps, rep_out)
                    :: (rep_out, Eps, prev)
                    :: sub_acc
                in
                (rep_acc, rep_in)
            | Concatenation (r1, r2) ->
                let r2acc, r2in = aux acc prev r2 in
                let r1acc, r1in = aux r2acc r2in r1 in
                (r1acc, r1in)
            | Alternation (r1, r2) ->
                let alt_out = get () in
                let alt_in = get () in
                let r2_out = get () in
                let r1_out = get () in
                let r2acc, r2in = aux acc r2_out r2 in
                let r1acc, r1in = aux r2acc r1_out r1 in
                let alt_acc =
                    (alt_in, Eps, r2in)
                    :: (alt_in, Eps, r1in)
                    :: (r1_out, Eps, alt_out)
                    :: (r2_out, Eps, alt_out)
                    :: (alt_out, Eps, prev)
                    :: r1acc
                in
                (alt_acc, alt_in)
    in
    aux [] _end exp


let to_string ((states, _) : nfa) : string =
    let lst =
        List.map
            (function
                | s1, Eps, s2 -> Printf.sprintf "\tS%d -> S%d" s1 s2
                | s1, Ch c, s2 -> Printf.sprintf "\tS%d -> S%d [label=\"%c\"]" s1 s2 c)
            states
    in
    let g = String.concat "\n" lst in
    let s =
        String.concat
            "\n"
            [ "digraph NFA {"
            ; "\trankdir=LR"
            ; "\tnode [shape=doublecircle]"
            ; "\tS0"
            ; "\tnode [shape=circle]"
            ; g
            ; "}" ]
    in
    s


let backtracking_match (pattern : string) (s : string) : bool =
    let ast = Parser.parse pattern in
    let states, state = to_nfa ast in
    (* print_endline (to_string (states, state)); *)
    let rec backtracking (states : states) (prev : state) (lst : char list) :
        bool =
        let rec aux = function
            | [] -> prev = _end && lst = []
            | (p, Eps, next) :: pt when p = prev ->
                backtracking states next lst || aux pt
            | (p, Ch c, next) :: pt when p = prev ->
                let m =
                    match lst with
                        | ch :: ct when ch = c -> backtracking states next ct
                        | _ -> false
                in
                m || aux pt
            | _ :: pt -> aux pt
        in
        aux states
    in
    backtracking states state (CCString.to_list s)
