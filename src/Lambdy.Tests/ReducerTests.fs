module Reducer.Tests

open NUnit.Framework
open FsUnit

open Lambdy.Lib.ParseTree
open Lambdy.Lib.IR
open Lambdy.Lib.LambdaTerm
open Lambdy.Lib.Reducer

[<Literal>]
let ReductionLimit = 1000

let irVarTerm = Name >> irVar
let lamVarTerm = Name >> lamVar

let irI name = irAbs (Name name) (irVarTerm name)
let lamI name = lamAbs (Name name) (lamVarTerm name)

let irOmega = irAbs (Name "x") (irApp (irVarTerm "x") (irVarTerm "x"))

let lamOmega = lamAbs (Name "x") (lamApp (lamVarTerm "x") (lamVarTerm "x"))

let irT = irAbs (Name "x") (irAbs (Name "y") (irVarTerm "x"))
let irF = irAbs (Name "x") (irAbs (Name "y") (irVarTerm "y"))

let irY =
    let y =
        irAbs (Name "x") (irApp (irVarTerm "f") (irApp (irVarTerm "x") (irVarTerm "x")))

    irAbs (Name "f") (irApp y y)

let terms =
    [
        // No redexes
        irVarTerm "x"
        irApp (irVarTerm "x") (irVarTerm "y")
        irAbs (Name "x") (irVarTerm "y")
        irOmega

        // Simple reduction
        irApp irOmega (irVarTerm "y")
        irApp (irAbs (Name "x") (irVarTerm "x")) (irVarTerm "z")
        irApp (irAbs (Name "x") (irApp (irVarTerm "x") (irVarTerm "y"))) (irVarTerm "z")
        irApp (irAbs (Name "x") (irVarTerm "y")) (irApp irOmega irOmega)

        // Inner reduction
        irAbs (Name "a") (irApp (irAbs (Name "b") (irApp (irVarTerm "b") (irVarTerm "c"))) (irVarTerm "x"))
        irApp (irVarTerm "x") (irApp (irAbs (Name "a") irOmega) (irVarTerm "z"))

        // Multiple variable substitution
        irApp
            (irApp (irAbs (Name "a") (irAbs (Name "b") (irApp (irVarTerm "a") (irVarTerm "b")))) (irVarTerm "x"))
            (irVarTerm "y")
        irApp
            (irApp
                (irApp (irAbs (Name "a") (irAbs (Name "b") (irAbs (Name "c") (irVarTerm "x")))) (irVarTerm "y"))
                (irVarTerm "y"))
            (irVarTerm "y")
        irApp
            (irApp
                (irApp
                    (irAbs
                        (Name "U")
                        (irAbs (Name "V") (irAbs (Name "W") (irAbs (Name "H") (irAbs (Name "K") (irVarTerm "W"))))))
                    (irVarTerm "a"))
                (irVarTerm "b"))
            (irVarTerm "c")
        irApp
            (irVarTerm "trololo")
            (irApp (irApp (irAbs (Name "x") (irAbs (Name "y") (irVarTerm "x"))) (irVarTerm "A")) (irVarTerm "B"))

        // Term substitution
        irApp (irI "IDENT") (irI "IDENT")
        irAbs (Name "a") (irApp (irAbs (Name "b") (irApp (irVarTerm "a") (irVarTerm "b"))) (irI "x"))
        irApp (irI "y") (irApp (irVarTerm "x") (irVarTerm "y"))
        irApp (irAbs (Name "a") (irAbs (Name "b") (irApp (irVarTerm "a") (irVarTerm "b")))) (irI "x")

        // Nested redex
        irApp (irApp (irI "x") (irAbs (Name "x") (irVarTerm "z"))) irY
        irApp irY irF
        irApp (irVarTerm "S") (irApp (irApp (irApp irT (irVarTerm "a")) (irVarTerm "b")) (irVarTerm "S"))
        irApp (irApp (irI "x") (irApp (irApp irF (irVarTerm "x")) (irI "x"))) (irVarTerm "y")

        // Irreducible term
        irApp irOmega irOmega
        irApp (irApp irOmega irOmega) (irApp irOmega irOmega)
        irY
        irApp irY (irVarTerm "x")

        // Alpha conversion required
        irApp (irAbs (Name "x") (irAbs (Name "y") (irApp (irVarTerm "x") (irVarTerm "y")))) (irVarTerm "y")
        irApp (irAbs (Name "z'") (irAbs (Name "z") (irApp (irVarTerm "z") (irVarTerm "z'")))) (irVarTerm "z")
        irApp
            (irAbs (Name "x") (irApp (irAbs (Name "y") (irApp (irVarTerm "x") (irVarTerm "y"))) (irVarTerm "x")))
            (irAbs (Name "y") (irApp (irVarTerm "x") (irVarTerm "y")))
        irApp
            (irAbs (Name "a") (irAbs (Name "b") (irAbs (Name "c") (irApp (irVarTerm "a") (irVarTerm "b")))))
            (irApp (irVarTerm "c") (irVarTerm "b"))
    ]

let reducedTerms =
    [
        // No redexes
        Some <| lamVarTerm "x"
        Some <| lamApp (lamVarTerm "x") (lamVarTerm "y")
        Some <| lamAbs (Name "x") (lamVarTerm "y")
        Some <| lamOmega

        // Simple reduction
        Some <| lamApp (lamVarTerm "y") (lamVarTerm "y")
        Some <| lamVarTerm "z"
        Some <| lamApp (lamVarTerm "z") (lamVarTerm "y")
        Some <| lamVarTerm "y"

        // Inner reduction
        Some <| lamAbs (Name "a") (lamApp (lamVarTerm "x") (lamVarTerm "c"))
        Some <| lamApp (lamVarTerm "x") lamOmega

        // Multiple variable substitution
        Some <| lamApp (lamVarTerm "x") (lamVarTerm "y")
        Some <| lamVarTerm "x"
        Some <| lamAbs (Name "H") (lamAbs (Name "K") (lamVarTerm "c"))
        Some <| lamApp (lamVarTerm "trololo") (lamVarTerm "A")

        // Term substitution
        Some <| lamI "IDENT"
        Some <| lamAbs (Name "a") (lamApp (lamVarTerm "a") (lamI "x"))
        Some <| lamApp (lamVarTerm "x") (lamVarTerm "y")
        Some <| lamAbs (Name "b") (lamVarTerm "b")

        // Nested redex
        Some <| lamVarTerm "z"
        Some <| lamAbs (Name "y") (lamVarTerm "y")
        Some <| lamApp (lamVarTerm "S") (lamApp (lamVarTerm "a") (lamVarTerm "S"))
        Some <| lamVarTerm "y"

        // Irreducible term
        None
        None
        None
        None

        // Alpha conversion required
        Some <| lamAbs (Name "y'") (lamApp (lamVarTerm "y") (lamVarTerm "y'"))
        Some <| lamAbs (Name "z''") (lamApp (lamVarTerm "z''") (lamVarTerm "z"))
        Some
        <| lamApp (lamVarTerm "x") (lamAbs (Name "y") (lamApp (lamVarTerm "x") (lamVarTerm "y")))
        Some
        <| lamAbs (Name "b'") (lamAbs (Name "c'") (lamApp (lamApp (lamVarTerm "c") (lamVarTerm "b")) (lamVarTerm "b'")))
    ]

let testCases = List.zip terms reducedTerms |> List.map TestCaseData

[<TestCaseSource(nameof (testCases))>]
let testReduce term (reducedTerm : LambdaTerm option) =
    let reducer = new Reducer (reductionLimit = ReductionLimit)
    let result = reducer.Reduce term

    match result with
    | Ok term -> term |> should equal reducedTerm.Value
    | Error _ -> reducedTerm.IsNone |> should be True
