open System
open System.IO

open LambdaInterpreter
open LambdaInterpreter.Console
open ColorScheme
open ExitCode
open Help

/// Print header with general info about the app and a help suggestion.
let printHeaderWithHelpSuggestion () =
    printHeader ()
    printHelpSuggestion ()

/// Print header with general info about the app followed by command line and interpreter usage help.
let printHeaderWithHelp () =
    printHeader ()
    printUsageHelp ()
    printOptionsHelp ()
    printSyntaxHelp ()

/// Print the given `message` to the standard output with the given `color`.
let printMessage (color: ConsoleColor) (message: string) =
    Console.ForegroundColor <- color
    printfn "%s" message
    Console.ResetColor()

/// Print the result of interpretation according to the given `output` using the given color `scheme`.
let handleOutput (Color success, Color error) (output: Result<string, string>) =
    match output with
    | Ok result -> printMessage success result
    | Error message -> printMessage error message

let options = Options.GetFromArgs()

if options.Help then
    printHeaderWithHelp ()
    exit <| int ExitCode.Success

if options.Error then
    printMessage defaultErrorColor invalidArgsMessage
    exit <| int ExitCode.InvalidArguments

let interpreter =
    match options.SourceFile with
    | Some path ->
        try
            Interpreter.StartOnFile(path, options.Verbose, options.LineNumber)
        with
        | :? IOException
        | :? UnauthorizedAccessException as ex ->
            printMessage defaultErrorColor ex.Message
            exit <| int ExitCode.FileNotFound
    | None -> Interpreter.StartInteractive options.Verbose

using interpreter (fun interpreter ->
    let colorScheme = getColorScheme interpreter

    if interpreter.IsInteractive then
        printHeaderWithHelpSuggestion ()

    for output in interpreter.RunToEnd() do
        handleOutput colorScheme output

    getExitCode interpreter |> int |> exit)
