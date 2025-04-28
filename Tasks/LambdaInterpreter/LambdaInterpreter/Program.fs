open System
open System.IO

open LambdaInterpreter
open ColorScheme
open ExitCode

/// Print header with general info about the app and a help suggestion.
let printHeader () =
    printfn "
Lambda Interpreter
------------------
A simple interpreter of lambda term expressions.

Type 'help' for more info.
"

/// Print command line help.
let printHelp () =
    printfn "
Lambda Interpreter
------------------
A simple interpreter of lambda term expressions.
It can either be run interactively using the standard input,
or on a specified source file.

Usage:
    LambdaInterpreter [options]\t\t Run interpreter interactively
    LambdaInterpreter {path-to-file}\t Run interpreter on the given source file

Options:
    -h|--help\t\t Display help and exit
"   Interpreter.PrintHelp ()

/// Print the given `message` to the standard output with the given `color`.
let printMessage (color: ConsoleColor) (message: string) =
    Console.ForegroundColor <- color
    printfn "%s" message
    Console.ResetColor ()

/// Print the result of interpretation according to the given `output` using the given color `scheme`.
let handleOutput (Color success, Color error) (output: Result<string, string>) =
    match output with
    | Ok result -> printMessage success (result + "\n")
    | Error message -> printMessage error message

let args = Environment.GetCommandLineArgs ()

if Array.contains "-h" args || Array.contains "--help" args then
    printHelp ()
    exit <| int ExitCode.Success

let interpreter =
    try
        if args.Length = 2 then Interpreter.StartOnFile (args[1], true)
        else Interpreter.StartInteractive true
    with
        | :? FileNotFoundException | :? DirectoryNotFoundException as ex ->
            printMessage ConsoleColor.Red (ex.Message + "\n")
            exit <| int ExitCode.FileNotFound

using interpreter (fun interpreter ->
    let colorScheme = getColorScheme interpreter
    if interpreter.IsInteractive then printHeader ()

    for output in interpreter.RunToEnd () do
        handleOutput colorScheme output

    getExitCode interpreter |> int |> exit
)
