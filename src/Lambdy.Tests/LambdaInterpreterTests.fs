namespace Lambdy.Tests

open System.IO

open NUnit.Framework
open FsUnit

open Lambdy.Lib.IR
open Lambdy.Lib.AST
open Lambdy.Lib.LambdaTerm
open Lambdy.Lib.Interfaces.IReducer
open Lambdy.Lib.Interfaces.ILambdaInterpreter
open Lambdy.Lib.LambdaInterpreter

module LambdaInterpreterTests =

    [<Literal>]
    let ReductionLimit = 10000

    // ============= Input data ===========================================

    [<Literal>]
    let testFilesDir = "TestFiles"

    let successfulCasesPath = Path.Join [| testFilesDir ; "Successful" |]
    let unsuccessfulCasesPath = Path.Join [| testFilesDir ; "Unsuccessful" |]

    let successfulCasesSources =
        Directory.EnumerateFiles successfulCasesPath |> seq |> Seq.sort

    let unsuccessfulCasesSources =
        Directory.EnumerateFiles unsuccessfulCasesPath |> seq |> Seq.sort

    // ============= Expected output ======================================

    let successfulCasesResults : (LineNumber * string option) list list =
        [
            [ // Test 1
                Line 3, Some "λz.z"
                Line 8, Some "trololo"
            ]
            [ // Test 2
                Line 4, Some "ololo"
                Line 7, Some "a"
                Line 9, Some "λV1.λV2.V2 λx.V2 x"
                Line 10, Some "λV1.λV2.V2 λx.V2 x"
                Line 11, Some "λV1.λV2.V2 (λx.V2) x"
                Line 12, Some "λV1.λV2.V2 (λx.V2) x"
                Line 13, Some "λx.λy.λx.x"
            ]
            [ // Test 3
                Line 1, Some "qwerty"
                Line 2, Some "ololo (A_ B_)"
                Line 3, Some "λx.λy.z λx.y x"
                Line 4, Some "Mult (Sum λP.P)"
                Line 12, Some "second third λx.x first"
                Line 15, Some "second third"
            ]
            [ // Test 4
                Line 1, Some "x y"
                Line 4, Some "x"
                Line 6, Some "λy'.y y'"
                Line 12, Some "a b c Y"
                Line 13, Some "λY'.Y λa'.a b c Y'"
                Line 14, Some "Y λa'.a b c (a b c)"
                Line 15, Some "Y λa'.a b c Y"
            ]
            [ // Test 5
                Line 4, Some "λy.ololo"
                Line 7, Some "snd"
                Line 8, Some "res"
                Line 11, Some "λU.λV.V λx.U"
                Line 12, Some "λleft.λright.left (λx.x) right"
                Line 13, Some "λx.λy.λz.z (z y x)"
            ]
            [ // Test 6
                Line 2, Some "a"
                Line 4, Some "λa.λb.b a"
                Line 7, Some "baz bar"
            ]
            [ // Test 7
                Line 3, Some "var"
                Line 5, Some "λx.x"
                Line 7, Some "exitCode"
                Line 8, Some "helpMe"
                Line 9, Some "toClear"
                Line 10, Some "reset_vars"
                Line 11, Some "no_exit_1"
            ]
            [] // Test 8
            [ // Test 9
                Line 8, Some "U"
                Line 9, Some "V"
                Line 10, Some "λf.f V U"
                Line 11, Some "V"
                Line 12, Some "U"
            ]
            [ // Test 10
                Line 13, Some <| irToString false false (churchNumeral 2)
                Line 14, Some <| irToString false false (churchNumeral 3)
                Line 15, Some <| irToString false false (churchNumeral 3)
                Line 16, Some <| irToString false false (churchNumeral 0)
                Line 18, Some <| irToString false false (churchNumeral 5)
                Line 19, Some <| irToString false false (churchNumeral 6)
                Line 20, Some <| irToString false false (churchNumeral 0)
                Line 21, Some <| irToString false false (churchNumeral 8)
            ]
            [ // Test 11
                Line 19, Some <| irToString false false (churchNumeral 1)
                Line 20, Some <| irToString false false (churchNumeral 1)
                Line 21, Some <| irToString false false (churchNumeral 2)
                Line 22, Some <| irToString false false (churchNumeral 6)
                Line 23, Some <| irToString false false (churchNumeral 24)
            ]
            [ // Test 12
                Line 20, Some <| irToString false false (churchNumeral 1)
                Line 21, Some <| irToString false false (churchNumeral 1)
                Line 22, Some <| irToString false false (churchNumeral 2)
                Line 23, Some <| irToString false false (churchNumeral 3)
                Line 24, Some <| irToString false false (churchNumeral 5)
                Line 25, Some <| irToString false false (churchNumeral 8)
            ]
        ]

    let unsuccessfulCasesResults : (LineNumber * string option) list list =
        [
            [ // Test 1
                Line 1, None
                Line 2, None
                Line 3, None
                Line 4, None
                Line 6, None
                Line 7, None
                Line 8, None
                Line 10, None
                Line 11, None
                Line 13, Some "res"
            ]
            [ // Test 2
                Line 1, None
                Line 2, None
                Line 3, None
                Line 4, None
                Line 5, None
                Line 6, None
                Line 8, None
                Line 9, None
                Line 10, None
                Line 11, None
                Line 12, None
                Line 13, None
            ]
            [ // Test 3
                Line 1, None
                Line 2, None
                Line 4, None
                Line 5, None
                Line 6, None
                Line 7, None
                Line 8, None
                Line 9, None
                Line 11, None
                Line 12, None
                Line 13, None
                Line 14, None
                Line 15, None
            ]
            [ // Test 4
                Line 3, Some "λx.λy.x"
                Line 4, Some "λx.λy.x"
                Line 8, None
                Line 10, Some "bar"
                Line 15, Some "foo bar"
            ]
            [ // Test 5
                Line 2, None
                Line 6, None
                Line 7, None
                Line 9, Some "λx.x x"
            ]
        ]

    // ============= Test cases ===========================================

    let successfulCases =
        successfulCasesResults
        |> Seq.zip successfulCasesSources
        |> Seq.map (fun (file, res) -> file, res, false)

    let unsuccessfulCases =
        unsuccessfulCasesResults
        |> Seq.zip unsuccessfulCasesSources
        |> Seq.map (fun (file, res) -> file, res, true)

    let testCases = Seq.append successfulCases unsuccessfulCases |> Seq.map TestCaseData

    // ============= Test callbacks =======================================

    let outputsMatch (expected : (LineNumber * string option) list) (actual : InterpreterOutput list) =
        actual.Length = expected.Length
        && Seq.zip actual expected
           |> Seq.map (
               function
               | Output (Line actualLine, Success (NormalForm actualRes, _)), (Line expectedLine, Some expectedRes) ->
                   actualLine = expectedLine && lamToString false false actualRes = expectedRes
               | Output (Line actualLine, Failure _), (Line expectedLine, None) -> actualLine = expectedLine
               | _ -> false
           )
           |> Seq.forall ((=) true)

    [<TestCaseSourceAttribute(nameof (testCases))>]
    let testInterpreter sourceFile expectedOutputs shouldFail =
        let interpreter =
            LambdaInterpreter.StartOnFile (path = sourceFile, etaConversions = false, reductionLimit = ReductionLimit)

        let outputs = interpreter.RunToEnd () |> Seq.toList
        outputs |> outputsMatch expectedOutputs |> should be True

        (interpreter.SyntaxError || interpreter.ReductionFailed)
        |> should equal shouldFail
