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

        "a|(b|c)*";
    ] in
    List.map (fun x ->
        Printf.printf "%10s\t" x;
        Printf.printf "%35s\t" (Parser.recursive_descent x |> Ast.to_string2);
        Printf.printf "%35s\t" (Parser.shunting_yard x |> Ast.to_string2);
        Printf.printf "%B\t" ((Parser.shunting_yard x) = (Parser.recursive_descent x));
        print_newline ()
    ) cases
