open System
open System.IO

open LambdaInterpreter
open ColorScheme
open ExitCode

/// Print generic info about the app with a help suggestion.
let printInfo () =
    printfn "
Lambda Interpreter
------------------
A simple interpreter of lambda term expressions.

For more info use -h option.
"

/// Print command line help.
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

/// Print a pointer indicating the start of input when running the interactive interpreter.
let printInputPointer () =
    printf "-> "

/// Print the given `message` to the standard output with the given `color`.
let printMessage (color: ConsoleColor) (message: string) =
    Console.ForegroundColor <- color
    printfn "%s" message
    Console.ResetColor ()

/// Print the result of interpretation according to the given `output` using the given color `scheme`.
let handleOutput (Color success, Color error) (output: Result<string, string>) =
    match output with
    | Ok result ->
        if result.Length > 0 then printMessage success (result + "\n")
    | Error message ->
        printMessage error message

let args = Environment.GetCommandLineArgs ()

if Array.contains "-h" args || Array.contains "--help" args then
    printHelp ()
    exit <| int ExitCode.Success

let interpreter =
    try
        if args.Length = 2 then new Interpreter (args.[1])
        else new Interpreter ()
    with
        | :? FileNotFoundException | :? DirectoryNotFoundException as ex ->
            printMessage ConsoleColor.Red (ex.Message + "\n")
            exit <| int ExitCode.FileNotFound

using interpreter (fun interpreter ->
    let colorScheme = getColorScheme interpreter
    if interpreter.IsInteractive then printInfo () ; printInputPointer ()

    for output in interpreter.RunToEnd () do
        handleOutput colorScheme output
        if interpreter.IsInteractive then printInputPointer ()

    getExitCode interpreter |> int |> exit
)
