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
            sub_in.next2 <- Some (Eps, sub_out);
            rep_out.next2 <- Some (Eps, rep_in);
            build_eps rep_in
        | Concatenation (r1, r2) ->
            let s2_in = aux state_out r2 in
            let s1_in = aux s2_in r1 in
            build_eps s1_in
        | Alternation (r1, r2) ->
            let alt_out = build_eps state_out in
            let s1_in = aux alt_out r1 in
            let s2_in = aux alt_out r2 in
            let alt_in = build_state s1_in s2_in in
            build_eps alt_in
            (* alt_in *)
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
    let edge_to_string acc e n s xx =
        let s = (match e with
            | Eps -> P.sprintf "S%d -> S%d[label=%d]" n s.n xx
            | Ch c -> P.sprintf "S%d -> S%d[label=%c%d]" n s.n c xx
        ) in s::acc
    in
    let rec aux acc s =
        match s with
            | {x=true;_} -> acc
            | {last=true;_} -> acc
            | {n;next1=Some(e1,s1);next2=None;_} ->
                s.x <- true;
                let prev = aux acc s1 in
                edge_to_string prev e1 n s1 1
            | {n;next1=Some(e1,s1);next2=Some(e2,s2);_} ->
                s.x <- true;
                let prev1 = aux acc s1 in
                let prev2 = aux prev1 s2 in
                let curr1 = edge_to_string prev2 e1 n s1 1 in
                edge_to_string curr1 e2 n s2 2
            | _ -> failwith "unexpected"
    in
    let content =
        aux [] nfa
        |> remove_duplicate
        |> lst_to_string
    in
    P.sprintf "digraph G {\n%s}" content


let explode s =
    let rec exp i l =
        if i < 0
        then l
        else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []

type eend = bool * bool (* input-end, matched *)
let backtracking_match (pattern:string) (s:string) : bool =
    let ast = Parser.parse pattern in
    let nfa = to_nfa ast in
    (* print_endline (to_string nfa); *)
    let clst = explode s in
    let rec backtracking state lst : bool =
        let _, e2 = aux state lst in
        e2
    and aux state lst : eend =
        match lst with
            | [] ->
                (match state with
                    | {x=true;_} -> true, false
                    | {last=true;_} -> true, true
                    | {next1=Some(Eps,n);next2=None;_} ->
                        state.x <- true;
                        if n.x = true then true, false else aux n []
                    | {next1=Some(Eps,n1);next2=Some(Eps,n2);_} ->
                        state.x <- true;
                        (match n1.x, n2.x with
                            | false, false -> true, ((aux n1 [])=(true,true) || (aux n2 [])=(true,true))
                            | false, true -> (aux n1 [])
                            | true, false -> (aux n2 [])
                            | true, true -> true, false)
                    | _ -> true, false)
            | h::t ->
                (match state with
                    | {last=true;_} -> false, true
                    | {next1=Some x;next2=None;_} ->
                        (match x with
                            | Eps, n -> aux n lst
                            | Ch c, n when c=h -> aux n t
                            | Ch _, _ -> false, false)
                    | {next1=Some (e1, n1);next2=Some (e2, n2);_} ->
                        (match n1.x, n2.x with
                            | false, false -> (
                                    (match (match e1 with
                                        | Eps -> aux n1 lst
                                        | Ch c when c=h -> aux n1 t
                                        | Ch _ -> false, false)
                                    with
                                        | true, x -> true, x
                                        | false, true -> false, true
                                        | false, false -> (
                                                (match e2 with
                                                    | Eps -> aux n2 lst
                                                    | Ch c when c=h -> aux n2 t
                                                    | Ch _ -> false, false)
                                            ))
                                )
                            | false, true ->
                                (match e1 with
                                    | Eps -> aux n1 lst
                                    | Ch c when c=h -> aux n1 t
                                    | Ch _ -> false, false)
                            | true, false ->
                                (match e2 with
                                    | Eps -> aux n2 lst
                                    | Ch c when c=h -> aux n2 t
                                    | Ch _ -> false, false)
                            | true, true -> false, false)
                    | _ -> failwith "never")
    in
    backtracking nfa clst
