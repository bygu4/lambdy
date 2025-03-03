module LambdaTerm.Tests

open NUnit.Framework
open FsUnit

let omega var = Abstraction (var, Application (Variable var, Variable var))
let Omega var = Application (omega var, omega var)

let terms =
    [
        // Simple terms
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
        Application (Variable "x", Application (Abstraction ("a", omega "y"), Variable "z"));

        // Multiple variables
        Application (Application
            (Abstraction ("a", Abstraction ("b", Application (Variable "a", Variable "b"))),
            Variable "x"), Variable "y");
        Application (Application (Application
            (Abstraction ("a", Abstraction ("b", Abstraction ("c", Variable "x"))),
            Variable "y"), Variable "y"), Variable "y");

        // Irreducible redex
        Omega "z";
        Application (Omega "x", Omega "y")
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

        Omega "z";
        Application (Omega "x", Omega "y")
    ]

let testCases = List.zip terms reducedTerms |> List.map TestCaseData

[<TestCaseSource(nameof(testCases))>]
let testReduce term reducedTerm =
    reduce term |> should equal reducedTerm
