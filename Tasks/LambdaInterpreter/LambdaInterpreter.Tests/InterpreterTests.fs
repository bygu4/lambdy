module Interpreter.Tests

open NUnit.Framework
open FsUnit
open System
open System.IO

open LambdaInterpreter

let testFilesDir = "TestFiles"
let successfulCasesPath = Path.Join [| testFilesDir; "Successful" |]
let unsuccessfulCasesPath = Path.Join [| testFilesDir; "Unsuccessful" |]

let successfulCasesSources = Directory.EnumerateFiles successfulCasesPath |> seq |> Seq.sort
let unsuccessfulCasesSources = Directory.EnumerateFiles unsuccessfulCasesPath |> seq |> Seq.sort

let e = String.Empty

let successfulCasesResults: Result<string, string> list list = [
    [   // Test 1
        Ok e;
        Ok e;
        Ok "\\z.z";
    ];
    [   // Test 2
        Ok e;
        Ok e;
        Ok e;
        Ok "ololo";
        Ok e;
        Ok e;
        Ok "a";
        Ok e;
        Ok "\\V1.\\V2.V2 (\\x.V2 x)";
        Ok "\\x.\\y.\\x.x";
        Ok e;
    ];
    [   // Test 3
        Ok e;
        Ok e;
        Ok e;
        Ok e;
        Ok e;
        Ok e;
        Ok "second third (\\x.x first)";
        Ok e;
        Ok e;
        Ok "second third"
        Ok e;
    ];
    [   // Test 4
        Ok "x y";
        Ok e;
        Ok e;
        Ok "x";
        Ok e;
        Ok "\\y'.y y'";
    ];
    [   // Test 5
        Ok e;
        Ok e;
        Ok e;
        Ok "\\y.ololo";
        Ok e;
        Ok e;
        Ok "snd";
        Ok "res";
    ];
    [   // Test 6
        Ok e;
        Ok "a";
        Ok e;
        Ok e;
    ];
    [   // Test 7
        Ok e;
        Ok e;
        Ok "var";
        Ok e;
        Ok "\\x.x";
        Ok e;
        Ok "exitCode";
        Ok "helpMe";
        Ok "toClear";
        Ok "reset_vars";
        Ok "no_exit_1";
        Ok e;
    ];
]

let unsuccessfulCasesResults: Result<string, string> list list = [
    [   // Test 1
        Error e;
        Error e;
        Error e;
        Error e;
        Ok e;
        Error e;
        Error e;
        Error e;
        Ok e;
        Error e;
        Error e;
        Ok e;
        Ok "res";
    ];
    [   // Test 2
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Ok e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Ok e;
    ];
    [   // Test 3
        Error e;
        Error e;
        Ok e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Ok e;
        Error e;
        Error e;
        Error e;
        Error e;
        Error e;
        Ok e;
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

let getOutput (interpreter: Interpreter) =
    interpreter.RunToEnd ()
    |> Seq.map Async.RunSynchronously
    |> Seq.toList

let outputsMatch (actual: Result<string, string> list) (expected: Result<string, string> list) =
    Seq.zip actual expected
    |> Seq.map (function
        | Ok res1, Ok res2 -> res1 = res2
        | Error _, Error _ -> true
        | Ok _, Error _ | Error _, Ok _ -> false
    ) |> Seq.forall (( = ) true)

[<TestCaseSourceAttribute(nameof(testCases))>]
let testInterpreter (sourceFile: string, expectedOutput: Result<string, string> list, shouldFail: bool) =
    let interpreter = Interpreter.StartOnFile sourceFile
    let output = getOutput interpreter
    output |> outputsMatch expectedOutput |> should be True
    interpreter.SyntaxError |> should equal shouldFail
