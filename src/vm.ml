open Ast
module P = Printf

type loc = int

and opcode = Accept | Match of char | Jump of loc | Split of loc * loc

and inst = (loc * opcode) list

let to_instruction (exp : re) : inst =
    let __i = ref 0 in
    let next () : loc =
        incr __i;
        !__i
    in
    let rec aux (exp : re) (nextloc : loc) : inst * loc =
        match exp with
            | Epsilon -> ([], nextloc)
            | Character c ->
                let inst = (nextloc, Match c) in
                ([inst], next ())
            | Repeation r ->
                let sub_start = next () in
                let sub_inst, sub_next = aux r sub_start in
                let r_out = next () in
                let split1 = (nextloc, Split (sub_start, r_out)) in
                let split2 = (sub_next, Split (sub_start, r_out)) in
                (List.concat [[split1]; sub_inst; [split2]], r_out)
            | Concatenation (r1, r2) ->
                let r1inst, r1next = aux r1 nextloc in
                let r2inst, r2next = aux r2 r1next in
                (List.concat [r1inst; r2inst], r2next)
            | Alternation (r1, r2) ->
                let r1start = next () in
                let r1inst, r1next = aux r1 r1start in
                let r2start = next () in
                let r2inst, r2next = aux r2 r2start in
                let split = (nextloc, Split (r1start, r2start)) in
                let jump = (r1next, Jump r2next) in
                (List.concat [[split]; r1inst; [jump]; r2inst], r2next)
    in
    let reinst, renext = aux exp 0 in
    let accept_inst = (renext, Accept) in
    List.concat [reinst; [accept_inst]]


let to_string (inst : inst) =
    inst
    |> List.map (function
        | loc, Accept -> P.sprintf "%d | Accept" loc
        | loc, Match c -> P.sprintf "%d | Match %c" loc c
        | loc, Jump l -> P.sprintf "%d | Jump %d" loc l
        | loc, Split (x, y) -> P.sprintf "%d | Split (%d, %d)" loc x y )
    |> String.concat "\n"


let vm_backtracking (pattern : string) (s : string) : bool =
    let ast = Parser.parse pattern in
    let inst = to_instruction ast in
    let rec _match (loc, op) s =
        match op with
            | Accept -> s = []
            | Match c ->
                (match s with
                    | h :: t when h = c -> _match (List.nth inst (loc + 1)) t
                    | _ -> false)
            | Jump l -> _match (List.nth inst l) s
            | Split (x, y) -> _match (List.nth inst x) s || _match (List.nth inst y) s
    in
    _match (List.hd inst) (CCString.to_list s)


let vm_nonbacktracking (pattern : string) (s : string) : bool =
    let ast = Parser.parse pattern in
    let inst = to_instruction ast in
    let get_inst = List.nth inst in
    let rec _match (curr : loc list) (next : loc list) s : bool =
        match curr with
            | [] -> (match s with [] -> false | _ :: t -> _match next [] t)
            | h :: t ->
                let _, op = get_inst h in
                (match op with
                    | Accept -> if s = [] then true else _match t next s
                    | Match c ->
                        (match s with
                            | sh :: _ when sh = c -> _match t ((h + 1) :: next) s
                            | _ -> _match t next s)
                    | Jump l -> _match (l :: t) next s
                    | Split (x, y) -> _match (x :: y :: t) next s)
    in
    _match [0] [] (CCString.to_list s)
