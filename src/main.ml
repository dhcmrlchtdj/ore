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

        "|";
        "ab|";
        "|ab";
        "||";
        "ab||";
        "|ab|";
        "||ab";

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
        "a(b)";
        "ab(cd)";
        "ab(cd)ef";
        "(ab)cd";

        "(a|b)c";
        "(a|bc)de";
        "a(b|c)";
        "a(b|c)d";
        "a(bc|cd)e";

        "(a)*";
        "(ab)*";
        "(a*)*";
        "(ab*)*cd";
        "ab(c*d)e";
        "(a*)*";
        "a(b*c)*";

        "a(b|c)*";
    ] in
    let test f2 case =
        let f1 = Parser.recursive_descent in
        let x = (try f1 case |> Ast.to_string with Failure s -> s) in
        let y = (try f2 case |> Ast.to_string with Failure s -> s) in
        let xx = (try f1 case |> Ast.simplify |> Ast.to_string with Failure s -> s) in
        let yy = (try f2 case |> Ast.simplify |> Ast.to_string with Failure s -> s) in
        Printf.printf "%15s \t %45s \t %45s \t %B\n" case x y (xx=yy)
    in
    (* List.iter (test Parser.shunting_yard) cases; *)
    (* List.iter (test Parser.precedence_climbing) cases; *)
    List.iter (test Parser.pratt) cases;
    ()
