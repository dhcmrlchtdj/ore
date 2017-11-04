type state = int * bool
and edge = char
and arrow = state * edge * state
and states = arrow list
and dfa = states * state

type nsl = Nfa.state list
and nd_map = (nsl * state) list
and cn_map = (char * nsl) list


let to_string ((states, _):dfa) : string =
    let lst = List.map (function (s1,_), c, (s2,_) ->
            Printf.sprintf "\tS%d -> S%d [label=\"%c\"]" s1 s2 c
        ) states in
    let g = String.concat "\n" lst in
    let e = states
            |> List.map (function x,_,y -> [x; y])
            |> List.flatten
            |> List.filter (function _,b -> b)
            |> List.sort_uniq compare
            |> List.map (function s,_ -> Printf.sprintf "\tS%d" s)
    in
    let s = String.concat "\n" [
            "digraph DFA {";
            "\trankdir=LR";
            "\tnode [shape=doublecircle]";
            (String.concat "\n" e);
            "\tnode [shape=circle]";
            g;
            "}";
        ] in
    s

let to_dfa ((states, state):Nfa.nfa) : dfa =
    let rec i = ref 0
    and get () = incr i; !i

    and get_epsilon (acc:nsl) (state:Nfa.state) : nsl =
        let direct =
            states
            |> List.filter (function
                | a,Nfa.Eps,_ when a = state -> true
                | _ -> false)
            |> List.map (function _,_,a -> a)
        in
        get_epsilons (state::acc) direct
    and get_epsilons (acc:nsl) (states:nsl) : nsl =
        match states with
            | [] -> List.sort_uniq compare acc
            | h::t ->
                let acc2 = get_epsilon acc h in
                get_epsilons acc2 t
    and get_arrows (acc:cn_map) (state_list:nsl) : cn_map =
        let rec append_to (acc:cn_map) (access:(char*Nfa.state) list) =
            match access with
                | [] -> acc
                | (c,s)::t ->
                    let acc2 = (match List.assoc_opt c acc with
                        | None -> (c,[s]) :: acc
                        | Some l -> (c, (s::l)) :: (List.remove_assoc c acc)
                    ) in
                    append_to acc2 t
        in
        match state_list with
            | [] ->
                let mapper = function c, states ->
                    c, (get_epsilons [] states)
                in
                List.map mapper acc
            | h::t ->
                let access = (
                    states
                    |> List.filter (function
                        | a,(Nfa.Ch _),_ when a = h -> true
                        | _ -> false)
                    |> List.map (function
                        | _,(Nfa.Ch c),a -> c, a
                        | _ -> failwith "never")
                ) in
                let acc2 = append_to acc access in
                get_arrows acc2 t

    and ndmap_extend (m:nd_map) (n:nsl) : nd_map =
        match List.assoc_opt n m with
            | Some _ -> m
            | None ->
                let dstate = get (), List.mem Nfa._end n in
                (n, dstate)::m
    and ndmap_find (m:nd_map) (n:nsl) : state =
        List.assoc n m

    and get_dfa (dfas:states) (ntod:nd_map) (nfas:nsl list) : (states * nd_map) =
        let rec aux (from:state) (dfas:states) (ntod:nd_map) (nexts:nsl list) : cn_map -> (states * nd_map * (nsl list)) = function
            | [] ->
                dfas, ntod, nexts
            | (c,n)::t when List.assoc_opt n ntod = None ->
                let dstate = (get (), List.mem Nfa._end n) in
                let dfas2 = (from,c,dstate)::dfas in
                let ntod2 = (n, dstate)::ntod in
                let nexts2 = n::nexts in
                aux from dfas2 ntod2 nexts2 t
            | (c,n)::t ->
                let dstate = List.assoc n ntod in
                let dfas2 = (from,c,dstate)::dfas in
                aux from dfas2 ntod nexts t
        in
        match nfas with
            | [] -> dfas, ntod
            | h::t ->
                let ntod_x = ndmap_extend ntod h in
                let dstate = ndmap_find ntod_x h in
                let nexts = get_arrows [] h in
                let dfas2, ntod2, nexts2 = aux dstate dfas ntod_x [] nexts in
                get_dfa dfas2 ntod2 (List.concat [t; nexts2])
    in

    let start = get_epsilon [] state in
    let dfa_states, ntod = get_dfa [] [] [start] in
    let in_state = List.assoc start ntod in
    dfa_states, in_state


let dfa_match (pattern:string) (s:string) : bool =
    let ast = Parser.parse pattern in
    let nfa = Nfa.to_nfa ast in
    (* print_endline (Nfa.to_string nfa); *)
    let (states, start) = to_dfa nfa in
    (* print_endline (to_string (states, start)); *)
    let rec _match (states:states) ((curr,_end):state) (lst:char list) : bool =
        let rec aux = function
            | [] ->
                _end && (lst = [])
            | ((prev, _), c, next)::pt when prev = curr ->
                let m = (
                    match lst with
                        | ch::ct when ch = c -> _match states next ct
                        | _ -> false
                ) in
                m || aux pt
            | _::pt -> aux pt
        in
        aux states
    in
    _match states start (Util.explode s)
