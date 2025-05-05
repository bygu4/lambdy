module Interpreter.Tests

open NUnit.Framework
open FsUnit
open System
open System.IO

open LambdaInterpreter

let testFilesDir = "TestFiles"
let successfulCasesPath = Path.Join [|testFilesDir; "Successful"|]
let unsuccessfulCasesPath = Path.Join [|testFilesDir; "Unsuccessful"|]

let successfulCasesSources = Directory.EnumerateFiles successfulCasesPath |> seq |> Seq.sort
let unsuccessfulCasesSources = Directory.EnumerateFiles unsuccessfulCasesPath |> seq |> Seq.sort

let e = String.Empty

let successfulCasesResults: Result<string, string> list list = [
    [   // Test 1
        Ok "\\z.z";
        Ok "trololo";
    ];
    [   // Test 2
        Ok "ololo";
        Ok "a";
        Ok "\\V1.\\V2.V2 \\x.V2 x";
        Ok "\\V1.\\V2.V2 \\x.V2 x";
        Ok "\\V1.\\V2.V2 (\\x.V2) x";
        Ok "\\V1.\\V2.V2 (\\x.V2) x";
        Ok "\\x.\\y.\\x.x";
    ];
    [   // Test 3
        Ok "qwerty";
        Ok "ololo (A_ B_)";
        Ok "\\x.\\y.z \\x.y x";
        Ok "Mult (Sum \\P.P)";
        Ok "second third \\x.x first";
        Ok "second third";
    ];
    [   // Test 4
        Ok "x y";
        Ok "x";
        Ok "\\y'.y y'";
        Ok "(\\x.x x) (\\x.x x) (\\x.x x) \\x.x x";
    ];
    [   // Test 5
        Ok "\\y.ololo";
        Ok "snd";
        Ok "res";
        Ok "\\U.\\V.V \\x.U";
        Ok "\\left.\\right.left (\\x.x) right";
        Ok "\\x.\\y.\\z.z (z y x)";
    ];
    [   // Test 6
        Ok "a";
        Ok "\\a.\\b.b a";
        Ok "baz bar";
    ];
    [   // Test 7
        Ok "var";
        Ok "\\x.x";
        Ok "exitCode";
        Ok "helpMe";
        Ok "toClear";
        Ok "reset_vars";
        Ok "no_exit_1";
    ];
    [   // Test 8
    ];
    [   // Test 9
        Ok "U";
        Ok "V";
        Ok "\\f.f V U";
        Ok "V";
        Ok "U";
    ];
    [   // Test 10
        Ok "\\f.\\x.f (f x)";
        Ok "\\f.\\x.f (f (f x))";
        Ok "\\f.\\x.f (f (f x))";
        Ok "\\f.\\x.x";
        Ok "\\f.\\x.f (f (f (f (f x))))";
        Ok "\\f.\\x.f (f (f (f (f (f x)))))";
        Ok "\\f.\\x.x";
        Ok "\\f.\\x.f (f (f (f (f (f (f (f x)))))))";
    ];
    [   // Test 11
        Ok "\\f.\\x.f x";
        Ok "\\f.\\x.f x";
        Ok "\\f.\\x.f (f x)";
        Ok "\\f.\\x.f (f (f (f (f (f x)))))";
        Ok "\\f.\\x.f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f x)))))))))))))))))))))))";
    ];
]

let unsuccessfulCasesResults: Result<string, string> list list = [
    [   // Test 1
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Ok "res";
    ];
    [   // Test 2
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
    ];
    [   // Test 3
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
    ];
    [   // Test 4
        Ok "\\x.\\y.x";
        Ok "\\x.\\y.x";
        Error e;
        Ok "bar";
        Ok "foo bar";
    ];
    [   // Test 5
        Error e;
        Ok "(\\x.x x) \\x.x x";
        Ok "(\\x.x x) (\\x.x x) ((\\x.x x) \\x.x x) ((\\x.x x) \\x.x x)";
    ];
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

let outputsMatch (actual: Result<string, string> list) (expected: Result<string, string> list) =
    actual.Length = expected.Length &&
    Seq.zip actual expected
    |> Seq.map (function
        | Ok res1, Ok res2 -> res1 = res2
        | Error _, Error _ -> true
        | Ok _, Error _ | Error _, Ok _ -> false
    ) |> Seq.forall (( = ) true)

[<TestCaseSourceAttribute(nameof(testCases))>]
let testInterpreter (sourceFile: string, expectedOutput: Result<string, string> list, shouldFail: bool) =
    let interpreter = Interpreter.StartOnFile sourceFile
    let output = interpreter.RunToEnd () |> Seq.toList
    output |> outputsMatch expectedOutput |> should be True
    (interpreter.SyntaxError || interpreter.MaxDepthExceeded) |> should equal shouldFail
