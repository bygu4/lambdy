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
    printfn $"
Lambda Interpreter
------------------
A simple interpreter of lambda term expressions.
It can either be run interactively using the standard input,
or on a specified source file.

Usage: LambdaInterpreter {{path-to-file?}} [options]

Options:
    {String.Join ('|', Options.HelpArgs)}\t\t Display help and exit
    {String.Join ('|', Options.VerboseArgs)}\t Use detailed output"
    Interpreter.PrintHelp ()

/// Message to print when failed to interpret the given command line args.
let invalidArgsMessage = "Invalid command line arguments provided. For more info use '--help' option."

/// Print the given `message` to the standard output with the given `color`.
let printMessage (color: ConsoleColor) (message: string) =
    Console.ForegroundColor <- color
    printfn "%s" message
    Console.ResetColor ()

/// Print the result of interpretation according to the given `output` using the given color `scheme`.
let handleOutput (Color success, Color error) (output: Result<string, string>) =
    match output with
    | Ok result -> printMessage success result
    | Error message -> printMessage error message

let options = Options.GetFromArgs ()

if options.Help then
    printHelp ()
    exit <| int ExitCode.Success

if options.Error then
    printMessage ConsoleColor.Red invalidArgsMessage
    exit <| int ExitCode.BadArguments

let interpreter =
    match options.SourceFile with
    | Some path ->
        try Interpreter.StartOnFile (path, options.Verbose)
        with :? FileNotFoundException | :? DirectoryNotFoundException as ex ->
            printMessage ConsoleColor.Red ex.Message
            exit <| int ExitCode.FileNotFound
    | None -> Interpreter.StartInteractive options.Verbose

using interpreter (fun interpreter ->
    let colorScheme = getColorScheme interpreter
    if interpreter.IsInteractive then printHeader ()

    for output in interpreter.RunToEnd () do
        handleOutput colorScheme output

    getExitCode interpreter |> int |> exit
)
