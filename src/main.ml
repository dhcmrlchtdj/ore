open Batteries
module P = Printf

let cases =
    [ (* ""; *)
        (* "a"; *)
        (* "ab"; *)
        (* "abc"; *)
        (* "a*"; *)
        (* "a*b"; *)
        (* "a*bc"; *)
        (* "a*b*"; *)
        (* "a*b*c"; *)
        (* "a*b*cd"; *)
        (* "ab*"; *)
        (* "ab*c"; *)
        (* "ab*cd"; *)
        (* "a|b"; *)
        (* "a|b|c"; *)
        (* "ab|cd"; *)
        (* "ab|cd|ef"; *)
        (* "|"; *)
        (* "ab|"; *)
        (* "|ab"; *)
        (* "||"; *)
        (* "ab||"; *)
        (* "|ab|"; *)
        (* "||ab"; *)
        (* "a*|b*"; *)
        (* "a*|b*c"; *)
        (* "a*|bc"; *)
        (* "a*b|c*d"; *)
        (* "ab*|c*d"; *)
        (* "ab*|c*d|ef*"; *)
        (* "()"; *)
        (* "(a)"; *)
        (* "(a)(b)"; *)
        (* "(ab)(cd)"; *)
        (* "(a)bc"; *)
        (* "a(b)"; *)
        (* "ab(cd)"; *)
        (* "ab(cd)ef"; *)
        (* "(ab)cd"; *)
        (* "(a|b)c"; *)
        (* "(a|bc)de"; *)
        (* "a(b|c)"; *)
        (* "a(b|c)d"; *)
        (* "a(bc|cd)e"; *)
        (* "(a)*"; *)
        (* "(ab)*"; *)
        (* "(a* )*"; *)
        (* "(ab* )*cd"; *)
        (* "ab(c*d)e"; *)
        (* "(a* )*"; *)
        (* "a(b*c)*"; *)
        (* "aa*"; *)
        (* "a*a"; *)
        (* "a*a*a"; *)
        (* "(a|b)*(a|b)"; *)
        (* "a*|a"; *)
        (* "a*|a*"; *)
        (* "(ab)|(ab)*"; *)
        (* "b|c"; *)
        (* "(b|c)*"; *)
        (* "a(b|c)*"; *)
        (* "(a* )*"; *)
        (* "a*(a*b)"; *)
        (* "(ba* )a*"; *)
        (* "|"; *)
        (* "a*|"; *)
        (* "|a*"; *)
        (* "a*|a*"; *)
        (* "a|a*"; *)
        (* "a*|a"; *)
        (* "a|b"; *)
        (* "(a|b)*"; *)
        (* "a*bc"; *)
        "a*b|a*"
      ; "abc|bc|cd|cddd|acc|ace" ]

let match_case =
    [ ("", "", true)
    ; ("", "a", false)
    ; ("a", "a", true)
    ; ("a", "b", false)
    ; ("a", "ba", false)
    ; ("a*", "a", true)
    ; ("a*", "b", false)
    ; ("a*", "ba", false)
    ; ("a*", "bb", false)
    ; ("a*bc", "aaabc", true)
    ; ("ab", "ab", true)
    ; ("ab", "abc", false)
    ; ("ab", "ac", false)
    ; ("a|b", "a", true)
    ; ("abc|b", "a", false)
    ; ("a|b", "b", true)
    ; ("a|b", "ab", false)
    ; ("a|b", "c", false)
    ; ("abab|abbb", "abbb", true)
    ; ("a(b|c)*", "abbb", true)
    ; ("a(b|c)*a", "abba", true)
    ; ("a*b", "a", false)
    ; ("a*bc", "abc", true)
    ; ("a*bc", "bc", true)
    ; ("a*bc", "ab", false)
    ; ("a(bc|d)", "ab", false)
    ; ("a*b|a*", "a", true) ]

let try_parse fn case = try fn case |> Ast.to_string with Failure s -> s

let test_parser () =
    List.iter
        (fun case ->
             let base = try_parse Parser.parse case in
             let rd = try_parse Parser.recursive_descent case in
             let sy = try_parse Parser.shunting_yard case in
             let pc = try_parse Parser.precedence_climbing case in
             P.printf "%45s\n" case ;
             P.printf "%45s\n" base ;
             P.printf "%45s\n" rd ;
             P.printf "%45s\n" sy ;
             P.printf "%45s\n" pc ;
             print_newline () )
        cases

let test_nfa () =
    List.iter
        (fun case ->
             let ast = Parser.parse case in
             let nfa = Nfa.to_nfa ast in
             print_endline case ;
             print_endline (Ast.to_string ast) ;
             print_endline (Nfa.to_string nfa) ;
             print_newline () )
        cases

let test_nfa_backtracking () =
    List.iter
        (fun (p, s, b) ->
             P.printf "%10s \t %10s \t %B\n" p s (b = Nfa.backtracking_match p s) )
        match_case

let test_dfa () =
    List.iter
        (fun case ->
             let ast = Parser.parse case in
             let nfa = Nfa.to_nfa ast in
             let dfa = Dfa.to_dfa nfa in
             print_endline case ;
             print_endline (Ast.to_string ast) ;
             print_endline (Nfa.to_string nfa) ;
             print_endline (Dfa.to_string dfa) ;
             print_newline () )
        cases

let test_dfa_match () =
    List.iter
        (fun (p, s, b) ->
             P.printf "%10s \t %10s \t %B\n" p s (b = Dfa.dfa_match p s) )
        match_case

let test_inst () =
    List.iter
        (fun case ->
             let ast = Parser.parse case in
             let inst = Vm.to_instruction ast in
             print_endline case ;
             print_endline (Ast.to_string ast) ;
             print_endline (Vm.to_string inst) ;
             print_newline () )
        cases

let test_vm_backtracking () =
    List.iter
        (fun (p, s, b) ->
             P.printf "%10s \t %10s \t %B\n" p s (b = Vm.vm_backtracking p s) )
        match_case

let test_vm_nonbacktracking () =
    List.iter
        (fun (p, s, b) ->
             P.printf "%10s \t %10s \t %B\n" p s (b = Vm.vm_nonbacktracking p s) )
        match_case

let () =
    (* test_parser (); *)
    (* test_nfa (); *)
    (* test_nfa_backtracking (); *)
    (* test_dfa (); *)
    (* test_dfa_match (); *)
    (* test_inst (); *)
    (* test_vm_backtracking (); *)
    test_vm_nonbacktracking () ; ()
