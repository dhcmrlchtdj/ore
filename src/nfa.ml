open Ast

type edge = Eps | Ch of char
and out = (edge * state) option
and state = {
    n: int;
    last: bool;
    mutable next1: out;
    mutable next2: out;
    mutable x: bool;
}
let i = ref 0
let build l e1 e2 = incr i; {n=(!i);last=l;next1=e1;next2=e2;x=false;}
let build_eps n = build false (Some (Eps, n)) None
let build_ch c n = build false (Some (Ch c, n)) None
let build_state n1 n2 = build false (Some (Eps, n1)) (Some (Eps, n2))


let to_nfa (exp:re) : state =
    i := 0;
    let rec aux (state_out:state) = function
        | Epsilon -> state_out
        | Character c -> build_ch c state_out
        | Repeation re ->
            let rep_out = build_eps state_out in
            let sub_out = build_eps rep_out in
            let sub_in = aux sub_out re in
            let rep_in = build_eps sub_in in
            sub_out.next2 <- Some (Eps, sub_in);
            rep_out.next2 <- Some (Eps, rep_in);
            rep_in
        | Concatenation (r1, r2) ->
            let s2_in = aux state_out r2 in
            let s1_in = aux s2_in r1 in
            s1_in
        | Alternation (r1, r2) ->
            let alt_out = build_eps state_out in
            let s1_in = aux alt_out r1 in
            let s2_in = aux alt_out r2 in
            let alt_in = build_state s1_in s2_in in
            alt_in
    in
    let end_state = build true None None in
    aux end_state exp

module P = Printf

let to_string nfa =
    let rec remove_duplicate = function
        | [] -> []
        | h::t -> h::(remove_duplicate (List.filter (fun x -> x<>h) t))
    in
    let rec lst_to_string = function
        | [] -> ""
        | h::t -> h ^ "\n" ^(lst_to_string t)
    in
    let edge_to_string acc e n s =
        let s = (match e with
            | Eps -> P.sprintf "S%d -> S%d" n s.n
            | Ch c -> P.sprintf "S%d -> S%d[label=%c]" n s.n c
        ) in s::acc
    in
    let rec aux acc s =
        match s with
            | {x=true;_} -> acc
            | {last=true;_} -> acc
            | {n;next1=Some(e1,s1);next2=None;_} ->
                s.x <- true;
                let prev = aux acc s1 in
                edge_to_string prev e1 n s1
            | {n;next1=Some(e1,s1);next2=Some(e2,s2);_} ->
                s.x <- true;
                let prev1 = aux acc s1 in
                let prev2 = aux prev1 s2 in
                let curr1 = edge_to_string prev2 e1 n s1 in
                edge_to_string curr1 e2 n s2
            | _ -> failwith "unexpected"
    in
    let content =
        aux [] nfa
        |> remove_duplicate
        |> lst_to_string
    in
    P.sprintf "digraph G {\n%s}" content
