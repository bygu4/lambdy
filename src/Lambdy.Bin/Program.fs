open System
open System.IO

open Lambdy.Lib.LambdaInterpreter
open Lambdy.Bin.ColorScheme
open Lambdy.Bin.ExitCode
open Lambdy.Bin.Help

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

let options = Options.GetFromArgs ()

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
            LambdaInterpreter.StartOnFile (path, options.Verbose, options.LineNumber)
        with
        | :? IOException
        | :? UnauthorizedAccessException as ex ->
            printMessage defaultErrorColor ex.Message
            exit <| int ExitCode.FileNotFound
    | None -> LambdaInterpreter.StartInteractive options.Verbose

using
    interpreter
    (fun interpreter ->
        let colorScheme = getColorScheme interpreter

        if interpreter.IsInteractive then
            printHeaderWithHelpSuggestion ()

        for output in interpreter.RunToEnd () do
            handleOutput colorScheme output

        getExitCode interpreter |> int |> exit
    )
