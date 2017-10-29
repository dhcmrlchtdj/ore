let () =
    let cases = [
        "";
        "a";
        "ab";
        "abc";

        "a*";
        "a*b";
        "a*bc";
        "a*b*";
        "a*b*c";
        "a*b*cd";
        "ab*";
        "ab*c";
        "ab*cd";

        "a|b";
        "a|b|c";
        "ab|cd";
        "ab|cd|ef";
        (* "|"; *)
        (* "ab|"; *)
        (* "|ab"; *)
        (* "||"; *)
        (* "ab||"; *)
        (* "|ab|"; *)
        (* "||ab"; *)

        "a*|b*";
        "a*|b*c";
        "a*|bc";
        "a*b|c*d";
        "ab*|c*d";
        "ab*|c*d|ef*";

        "()";
        "(a)";
        "(a)(b)";
        "(ab)(cd)";
        "(a)bc";
        "ab(cd)";
        "ab(cd)ef";
        "(ab)cd";

        "(a)*";
        "(ab)*";
        "(a*)*";
        "(ab*)*ab";

        "a(b|c)*";
    ] in
    let test fn case =
        let rd = Parser.recursive_descent case |> Ast.to_string in
        let s = (try fn case |> Ast.to_string with Failure s -> s) in
        Printf.printf "%15s \t %45s \t %45s \t %B\n" case rd s (rd = s)
    in
    List.iter (test Parser.shunting_yard) cases;
    (* List.iter (test Parser.precedence_climbing) cases; *)
    ()
