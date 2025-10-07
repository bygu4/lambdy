module LambdaInterpreter.Tests

open NUnit.Framework
open FsUnit
open System
open System.IO

open Lambdy.Lib.LambdaTerm
open Lambdy.Lib.Interfaces.ILambdaInterpreter
open Lambdy.Lib.LambdaInterpreter

[<Literal>]
let ReductionLimit = 1000

[<Literal>]
let testFilesDir = "TestFiles"

let successfulCasesPath = Path.Join [| testFilesDir ; "Successful" |]
let unsuccessfulCasesPath = Path.Join [| testFilesDir ; "Unsuccessful" |]

let successfulCasesSources =
    Directory.EnumerateFiles successfulCasesPath |> seq |> Seq.sort

let unsuccessfulCasesSources =
    Directory.EnumerateFiles unsuccessfulCasesPath |> seq |> Seq.sort

let successfulCasesResults : (LineNumber * string option) list list =
    [
        [ // Test 1
            Line 3, Some "\\z.z"
            Line 8, Some "trololo"
        ]
        [ // Test 2
            Line 4, Some "ololo"
            Line 7, Some "a"
            Line 9, Some "\\V1.\\V2.V2 \\x.V2 x"
            Line 10, Some "\\V1.\\V2.V2 \\x.V2 x"
            Line 11, Some "\\V1.\\V2.V2 (\\x.V2) x"
            Line 12, Some "\\V1.\\V2.V2 (\\x.V2) x"
            Line 13, Some "\\x.\\y.\\x.x"
        ]
        [ // Test 3
            Line 1, Some "qwerty"
            Line 2, Some "ololo (A_ B_)"
            Line 3, Some "\\x.\\y.z \\x.y x"
            Line 4, Some "Mult (Sum \\P.P)"
            Line 12, Some "second third \\x.x first"
            Line 15, Some "second third"
        ]
        [ // Test 4
            Line 1, Some "x y"
            Line 4, Some "x"
            Line 6, Some "\\y'.y y'"
            Line 12, Some "a b c Y"
            Line 13, Some "\\Y'.Y \\a'.a b c Y'"
            Line 14, Some "Y \\a'.a b c (a b c)"
            Line 15, Some "Y \\a'.a b c Y"
        ]
        [ // Test 5
            Line 4, Some "\\y.ololo"
            Line 7, Some "snd"
            Line 8, Some "res"
            Line 11, Some "\\U.\\V.V \\x.U"
            Line 12, Some "\\left.\\right.left (\\x.x) right"
            Line 13, Some "\\x.\\y.\\z.z (z y x)"
        ]
        [ // Test 6
            Line 2, Some "a"
            Line 4, Some "\\a.\\b.b a"
            Line 7, Some "baz bar"
        ]
        [ // Test 7
            Line 3, Some "var"
            Line 5, Some "\\x.x"
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
            Line 10, Some "\\f.f V U"
            Line 11, Some "V"
            Line 12, Some "U"
        ]
        [ // Test 10
            Line 13, Some "\\f.\\x.f (f x)"
            Line 14, Some "\\f.\\x.f (f (f x))"
            Line 15, Some "\\f.\\x.f (f (f x))"
            Line 16, Some "\\f.\\x.x"
            Line 18, Some "\\f.\\x.f (f (f (f (f x))))"
            Line 19, Some "\\f.\\x.f (f (f (f (f (f x)))))"
            Line 20, Some "\\f.\\x.x"
            Line 21, Some "\\f.\\x.f (f (f (f (f (f (f (f x)))))))"
        ]
        [ // Test 11
            Line 19, Some "\\f.\\x.f x"
            Line 20, Some "\\f.\\x.f x"
            Line 21, Some "\\f.\\x.f (f x)"
            Line 22, Some "\\f.\\x.f (f (f (f (f (f x)))))"
            Line 23,
            Some
                "\\f.\\x.f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f x)))))))))))))))))))))))"
        ]
        [ // Test 12
            Line 20, Some "\\f.\\x.f x"
            Line 21, Some "\\f.\\x.f x"
            Line 22, Some "\\f.\\x.f (f x)"
            Line 23, Some "\\f.\\x.f (f (f x))"
            Line 24, Some "\\f.\\x.f (f (f (f (f x))))"
            Line 25, Some "\\f.\\x.f (f (f (f (f (f (f (f x)))))))"
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
            Line 3, Some "\\x.\\y.x"
            Line 4, Some "\\x.\\y.x"
            Line 8, None
            Line 10, Some "bar"
            Line 15, Some "foo bar"
        ]
        [ // Test 5
            Line 2, None
            Line 6, None
            Line 7, None
            Line 9, Some "\\x.x x"
        ]
    ]

let successfulCases =
    successfulCasesResults
    |> Seq.zip successfulCasesSources
    |> Seq.map (fun (file, res) -> file, res, false)

let unsuccessfulCases =
    unsuccessfulCasesResults
    |> Seq.zip unsuccessfulCasesSources
    |> Seq.map (fun (file, res) -> file, res, true)

let testCases = Seq.append successfulCases unsuccessfulCases |> Seq.map TestCaseData

let outputsMatch (expected : (LineNumber * string option) list) (actual : InterpreterOutput list) =
    actual.Length = expected.Length
    && Seq.zip actual expected
       |> Seq.map (
           function
           | Output (Line actualLine, Ok actualRes), (Line expectedLine, Some expectedRes) ->
               actualLine = expectedLine && toString false actualRes = expectedRes
           | Output (Line actualLine, Error _), (Line expectedLine, None) -> actualLine = expectedLine
           | _ -> false
       )
       |> Seq.forall ((=) true)

[<TestCaseSourceAttribute(nameof (testCases))>]
let testInterpreter (sourceFile : string, expectedOutput : (LineNumber * string option) list, shouldFail : bool) =
    let interpreter =
        LambdaInterpreter.StartOnFile (sourceFile, reductionLimit = ReductionLimit)

    let output = interpreter.RunToEnd () |> Seq.toList
    output |> outputsMatch expectedOutput |> should be True

    (interpreter.SyntaxError || interpreter.ReductionFailed)
    |> should equal shouldFail
