module LambdaTerm.Tests

open NUnit.Framework
open FsUnit

let id var = Abstraction (var, Variable var)
let omega var = Abstraction (var, Application (Variable var, Variable var))
let Omega var = Application (omega var, omega var)

let terms =
    [
        // No redexes
        Variable "x";
        Application (Variable "x", Variable "y");
        Abstraction ("x", Variable "y");
        omega "x";

        // Simple reduction
        Application (omega "x", Variable "y");
        Application (Abstraction ("x", Variable "x"), Variable "z");
        Application (Abstraction ("x", Application (Variable "x", Variable "y")), Variable "z");
        Application (Abstraction ("x", Variable "y"), Omega "x");

        // Inner reduction
        Abstraction ("a", Application
            (Abstraction ("b", Application (Variable "b", Variable "c")),
            Variable "x"));
        Application (Variable "x", Application
            (Abstraction ("a", omega "y"),
            Variable "z"));

        // Multiple variable substitution
        Application (Application
            (Abstraction ("a", Abstraction ("b", Application (Variable "a", Variable "b"))),
            Variable "x"), Variable "y");
        Application (Application (Application
            (Abstraction ("a", Abstraction ("b", Abstraction ("c", Variable "x"))),
            Variable "y"), Variable "y"), Variable "y");

        // Term substitution
        Application (id "x", id "y");
        Abstraction ("a", Application
            (Abstraction ("b", Application (Variable "a", Variable "b")),
            id "x"));
        Application (id "x", Application (Variable "x", Variable "y"));
        Application
            (Abstraction ("a", Abstraction ("b", Application (Variable "a", Variable "b"))),
            id "x");

        // Irreducible redex
        Omega "z";
        Application (Omega "x", Omega "y");

        // Alpha conversion required
        Application
            (Abstraction ("x", Abstraction ("y", Application (Variable "x", Variable "y"))),
            Variable "y");
        Application
            (Abstraction ("z'", Abstraction ("z", Application (Variable "z", Variable "z'"))),
            Variable "z");
        Application
            (Abstraction ("x", Application
                (Abstraction ("y", Application (Variable "x", Variable "y")),
                Variable "x")),
            Abstraction ("y", Application (Variable "x", Variable "y")));
        Application
            (Abstraction ("a", Abstraction ("b",
                Abstraction ("c", Application (Variable "a", Variable "b")))),
            Application (Variable "c", Variable "b"));
    ]

let reducedTerms =
    [
        Variable "x";
        Application (Variable "x", Variable "y");
        Abstraction ("x", Variable "y");
        omega "x";

        Application (Variable "y", Variable "y");
        Variable "z";
        Application (Variable "z", Variable "y");
        Variable "y";

        Abstraction ("a", Application (Variable "x", Variable "c"));
        Application (Variable "x", omega "y");

        Application (Variable "x", Variable "y");
        Variable "x";

        id "y";
        Abstraction ("a", Application(Variable "a", id "x"));
        Application (Variable "x", Variable "y");
        id "b";

        Omega "z";
        Application (Omega "x", Omega "y");

        Abstraction ("y'", Application (Variable "y", Variable "y'"));
        Abstraction ("z''", Application (Variable "z''", Variable "z"));
        Application (Variable "x", Abstraction ("y", Application (Variable "x", Variable "y")));
        Abstraction ("b'", Abstraction ("c'", Application
            (Application (Variable "c", Variable "b"),
            Variable "b'")));
    ]

let testCases = List.zip terms reducedTerms |> List.map TestCaseData

[<TestCaseSource(nameof(testCases))>]
let testReduce term reducedTerm =
    reduce term |> should equal reducedTerm
