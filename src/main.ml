let _ =
    let cases = [
        ""      ;
        "a|"    ;
        "|b"    ;
        "|"     ;
        "||"    ;
        "(a||)b";
        "(a|)b" ;
        "(|a)b" ;
        "a"     ;
        "abc"   ;
        "a|b"   ;
        "a|b|c" ;
        "(ab)c" ;
        "a(bc)" ;

        "a(b|c)*";
        "a*b*";
        "a*|b*";
    ] in
    List.map (fun x ->
        Printf.printf "'%s'\t\t" x;
        Printf.printf "'%s'\t\t" (Parser.recursive_descent x |> Ast.to_string);
        (* Printf.printf "'%s'\t\t" (Parser.shunting_yard x |> Ast.to_string); *)
        print_newline ()
    ) cases
