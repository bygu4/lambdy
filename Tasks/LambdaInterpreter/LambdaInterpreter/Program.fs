open System

open LambdaInterpreter

let printInfo () =
    printfn "
Lambda Interpreter
------------------
A simple interpreter of lambda term expressions.

For more info use -h option.
"

let printHelp () =
    printfn "
Lambda Interpreter
------------------
A simple interpreter of lambda term expressions.

Usage:
    LambdaInterpreter [options]\t\t Run interpreter interactively
    LambdaInterpreter {path-to-file}\t Run interpreter on the given source file

Options:
    -h|--help\t\t Display help

Syntax:
    variable\t\t [letter][letter|digit|_]*
    term\t\t {variable}|{abstraction}|{application}|({term})
    application\t\t {term} {term}
    abstraction\t\t \\{variables}.{term}
    definition\t\t let {variable} = {term}

Examples:
    let S = \\x y z.x z (y z)
    let K = \\x y.x
    S K K
"

type ExitCode =
    | Success = 0
    | Error = 1

/// Run the given `interpreter` to the end of stream.
let runInterpreter (interpreter: Interpreter) =
    while not interpreter.EndOfStream do
        async {
            if interpreter.IsInteractive then printf "-> "
            let! output = interpreter.RunOnNextLineAsync ()
            do
                match output with
                | Ok message | Error message -> printfn "%s" message
        } |> Async.RunSynchronously

let getExitCode (interpreter: Interpreter) =
    if interpreter.IsInteractive || not interpreter.HadError then ExitCode.Success
    else ExitCode.Error

let args = Environment.GetCommandLineArgs ()

if Array.contains "-h" args || Array.contains "--help" args then
    printHelp ()
    exit 0

printInfo ()

let interpreter  =
    if args.Length = 1 then new Interpreter (args.[0])
    else new Interpreter ()

using interpreter runInterpreter

getExitCode interpreter |> int |> exit
