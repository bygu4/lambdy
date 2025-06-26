module Reducer.Tests

open NUnit.Framework
open FsUnit

open Lambdy.Syntax.ParseTree
open Lambdy.Syntax.LambdaTerm
open Lambdy.Interpreter.Reducer

let var = Name >> Variable

let I = Abstraction (Name "x", var "x")
let omega = Abstraction (Name "x", Application (var "x", var "x"))
let Omega = Application (omega, omega)
let T = Abstraction (Name "x", Abstraction (Name "y", var "x"))
let F = Abstraction (Name "x", Abstraction (Name "y", var "y"))

let y =
    Abstraction (Name "x", Application (var "f", Application (var "x", var "x")))

let Y = Abstraction (Name "f", Application (y, y))

let terms =
    [
        // No redexes
        var "x"
        Application (var "x", var "y")
        Abstraction (Name "x", var "y")
        omega

        // Simple reduction
        Application (omega, var "y")
        Application (Abstraction (Name "x", var "x"), var "z")
        Application (Abstraction (Name "x", Application (var "x", var "y")), var "z")
        Application (Abstraction (Name "x", var "y"), Omega)

        // Inner reduction
        Abstraction (Name "a", Application (Abstraction (Name "b", Application (var "b", var "c")), var "x"))
        Application (var "x", Application (Abstraction (Name "a", omega), var "z"))

        // Multiple variable substitution
        Application (
            Application (Abstraction (Name "a", Abstraction (Name "b", Application (var "a", var "b"))), var "x"),
            var "y"
        )
        Application (
            Application (
                Application (Abstraction (Name "a", Abstraction (Name "b", Abstraction (Name "c", var "x"))), var "y"),
                var "y"
            ),
            var "y"
        )
        Application (
            Application (
                Application (
                    Abstraction (
                        Name "U",
                        Abstraction (
                            Name "V",
                            Abstraction (Name "W", Abstraction (Name "H", Abstraction (Name "K", var "W")))
                        )
                    ),
                    var "a"
                ),
                var "b"
            ),
            var "c"
        )
        Application (
            var "trololo",
            Application (Application (Abstraction (Name "x", Abstraction (Name "y", var "x")), var "A"), var "B")
        )

        // Term substitution
        Application (I, I)
        Abstraction (Name "a", Application (Abstraction (Name "b", Application (var "a", var "b")), I))
        Application (I, Application (var "x", var "y"))
        Application (Abstraction (Name "a", Abstraction (Name "b", Application (var "a", var "b"))), I)

        // Nested redex
        Application (Application (I, Abstraction (Name "x", var "z")), Y)
        Application (Y, F)
        Application (var "S", Application (Application (Application (T, var "a"), var "b"), var "S"))
        Application (Application (I, Application (Application (F, var "x"), I)), var "y")

        // Irreducible redex
        Omega
        Application (Omega, Omega)

        // Alpha conversion required
        Application (Abstraction (Name "x", Abstraction (Name "y", Application (var "x", var "y"))), var "y")
        Application (Abstraction (Name "z'", Abstraction (Name "z", Application (var "z", var "z'"))), var "z")
        Application (
            Abstraction (Name "x", Application (Abstraction (Name "y", Application (var "x", var "y")), var "x")),
            Abstraction (Name "y", Application (var "x", var "y"))
        )
        Application (
            Abstraction (Name "a", Abstraction (Name "b", Abstraction (Name "c", Application (var "a", var "b")))),
            Application (var "c", var "b")
        )
    ]

let reducedTerms =
    [
        // No redexes
        var "x"
        Application (var "x", var "y")
        Abstraction (Name "x", var "y")
        omega

        // Simple reduction
        Application (var "y", var "y")
        var "z"
        Application (var "z", var "y")
        var "y"

        // Inner reduction
        Abstraction (Name "a", Application (var "x", var "c"))
        Application (var "x", omega)

        // Multiple variable substitution
        Application (var "x", var "y")
        var "x"
        Abstraction (Name "H", Abstraction (Name "K", var "c"))
        Application (var "trololo", var "A")

        // Term substitution
        I
        Abstraction (Name "a", Application (var "a", I))
        Application (var "x", var "y")
        Abstraction (Name "b", var "b")

        // Nested redex
        var "z"
        Abstraction (Name "y", var "y")
        Application (var "S", Application (var "a", var "S"))
        var "y"

        // Irreducible redex
        Omega
        Application (Omega, Omega)

        // Alpha conversion required
        Abstraction (Name "y'", Application (var "y", var "y'"))
        Abstraction (Name "z''", Application (var "z''", var "z"))
        Application (var "x", Abstraction (Name "y", Application (var "x", var "y")))
        Abstraction (Name "b'", Abstraction (Name "c'", Application (Application (var "c", var "b"), var "b'")))
    ]

let testCases = List.zip terms reducedTerms |> List.map TestCaseData

[<TestCaseSource(nameof (testCases))>]
let testReduce term reducedTerm =
    let reducer = new Reducer ()
    reducer.Reduce term |> should equal reducedTerm
