let _ =
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

        "a|(b|c)*";
    ] in
    List.map (fun x ->
        Printf.printf "%10s\t" x;
        Printf.printf "%35s\t" (Parser.recursive_descent x |> Ast.to_string);
        (* Printf.printf "%35s\t" (Parser.recursive_descent x |> Ast.simplify |> Ast.to_string); *)
        (* Printf.printf "%35s\t" (Parser.shunting_yard x |> Ast.to_string); *)
        (* Printf.printf "%B\t" ((Parser.shunting_yard x |> Ast.simplify) = (Parser.recursive_descent x |> Ast.simplify)); *)
        Printf.printf "%35s\t" (Parser.precedence_climbing x |> Ast.to_string);
        Printf.printf "%B\t" ((Parser.precedence_climbing x |> Ast.simplify) = (Parser.recursive_descent x |> Ast.simplify));
        print_newline ()
    ) cases
