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
        if args.Length = 2 then Interpreter.StartOnFile (args[1])
        else Interpreter.StartInteractive ()
    with
        | :? FileNotFoundException | :? DirectoryNotFoundException as ex ->
            printMessage ConsoleColor.Red (ex.Message + "\n")
            exit <| int ExitCode.FileNotFound

async {
    use interpreter = interpreter
    let colorScheme = getColorScheme interpreter
    if interpreter.IsInteractive then printHeader () ; printInputPointer ()

    for nextLine in interpreter.RunToEnd () do
        let! output = nextLine
        handleOutput colorScheme output
        if interpreter.IsInteractive then printInputPointer ()

    getExitCode interpreter |> int |> exit
} |> Async.RunSynchronously
