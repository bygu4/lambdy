open System

open LambdaInterpreter.Interpreter

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

let args = Environment.GetCommandLineArgs ()

if Array.contains "-h" args || Array.contains "--help" args then
    printHelp ()
    exit 0

printInfo ()
let output = runInterpreterInConsole ()

let hasErrors = output |> Seq.exists (function | Error _ -> true | Ok _ -> false)
let exitCode = if hasErrors then 1 else 0

exit exitCode
