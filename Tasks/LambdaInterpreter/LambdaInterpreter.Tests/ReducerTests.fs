module Reduction.Tests

open NUnit.Framework
open FsUnit

open LambdaInterpreter
open AST

let id var = Abstraction (var, Variable var)
let omega var = Abstraction (var, Application (Variable var, Variable var))
let Omega var = Application (omega var, omega var)

let terms =
    [
        // No redexes
        Variable (Name "x");
        Application (Variable (Name "x"), Variable (Name "y"));
        Abstraction (Name "x", Variable (Name "y"));
        omega (Name "x");

        // Simple reduction
        Application (omega (Name "x"), Variable (Name "y"));
        Application (Abstraction (Name "x", Variable (Name "x")), Variable (Name "z"));
        Application (Abstraction (Name "x", Application (Variable (Name "x"), Variable (Name "y"))), Variable (Name "z"));
        Application (Abstraction (Name "x", Variable (Name "y")), Omega (Name "x"));

        // Inner reduction
        Abstraction (Name "a", Application
            (Abstraction (Name "b", Application (Variable (Name "b"), Variable (Name "c"))),
            Variable (Name "x")));
        Application (Variable (Name "x"), Application
            (Abstraction (Name "a", omega (Name "y")),
            Variable (Name "z")));

        // Multiple variable substitution
        Application (Application
            (Abstraction (Name "a", Abstraction (Name "b", Application (Variable (Name "a"), Variable (Name "b")))),
            Variable (Name "x")), Variable (Name "y"));
        Application (Application (Application
            (Abstraction (Name "a", Abstraction (Name "b", Abstraction (Name "c", Variable (Name "x")))),
            Variable (Name "y")), Variable (Name "y")), Variable (Name "y"));

        // Term substitution
        Application (id (Name "x"), id (Name "y"));
        Abstraction (Name "a", Application
            (Abstraction (Name "b", Application (Variable (Name "a"), Variable (Name "b"))),
            id (Name "x")));
        Application (id (Name "x"), Application (Variable (Name "x"), Variable (Name "y")));
        Application
            (Abstraction (Name "a", Abstraction (Name "b", Application (Variable (Name "a"), Variable (Name "b")))),
            id (Name "x"));

        // Irreducible redex
        Omega (Name "z");
        Application (Omega (Name "x"), Omega (Name "y"));

        // Alpha conversion required
        Application
            (Abstraction (Name "x", Abstraction (Name "y", Application (Variable (Name "x"), Variable (Name "y")))),
            Variable (Name "y"));
        Application
            (Abstraction (Name "z'", Abstraction (Name "z", Application (Variable (Name "z"), Variable (Name "z'")))),
            Variable (Name "z"));
        Application
            (Abstraction (Name "x", Application
                (Abstraction (Name "y", Application (Variable (Name "x"), Variable (Name "y"))),
                Variable (Name "x"))),
            Abstraction (Name "y", Application (Variable (Name "x"), Variable (Name "y"))));
        Application
            (Abstraction (Name "a", Abstraction (Name "b",
                Abstraction (Name "c", Application (Variable (Name "a"), Variable (Name "b"))))),
            Application (Variable (Name "c"), Variable (Name "b")));
    ]

let reducedTerms =
    [
        Variable (Name "x");
        Application (Variable (Name "x"), Variable (Name "y"));
        Abstraction (Name "x", Variable (Name "y"));
        omega (Name "x");

        Application (Variable (Name "y"), Variable (Name "y"));
        Variable (Name "z");
        Application (Variable (Name "z"), Variable (Name "y"));
        Variable (Name "y");

        Abstraction (Name "a", Application (Variable (Name "x"), Variable (Name "c")));
        Application (Variable (Name "x"), omega (Name "y"));

        Application (Variable (Name "x"), Variable (Name "y"));
        Variable (Name "x");

        id (Name "y");
        Abstraction (Name "a", Application(Variable (Name "a"), id (Name "x")));
        Application (Variable (Name "x"), Variable (Name "y"));
        id (Name "b");

        Omega (Name "z");
        Application (Omega (Name "x"), Omega (Name "y"));

        Abstraction (Name "y'", Application (Variable (Name "y"), Variable (Name "y'")));
        Abstraction (Name "z''", Application (Variable (Name "z''"), Variable (Name "z")));
        Application (Variable (Name "x"), Abstraction (Name "y", Application (Variable (Name "x"), Variable (Name "y"))));
        Abstraction (Name "b'", Abstraction (Name "c'", Application
            (Application (Variable (Name "c"), Variable (Name "b")),
            Variable (Name "b'"))));
    ]

let testCases = List.zip terms reducedTerms |> List.map TestCaseData

[<TestCaseSource(nameof(testCases))>]
let testReduce term reducedTerm =
    let reducer = new Reducer ()
    reducer.Reduce term |> should equal reducedTerm
