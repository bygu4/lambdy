open System
open System.IO

open Lambdy.Lib.LambdaInterpreter
open Lambdy.Bin.Verbosity
open Lambdy.Bin.ExitCode
open Lambdy.Bin.ColorScheme
open Lambdy.Bin.Options
open Lambdy.Bin.Help
open Lambdy.Bin.Logger

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

let checkForError (options : Options) =
    if options.Error then
        Logger.PrintError invalidArgsMessage
        exit <| int ExitCode.InvalidArguments
    else
        options

let tryPrintHelp (options : Options) =
    if options.Help then
        printHeaderWithHelp ()
        exit <| int ExitCode.Success
    else
        options

let createInterpreter (options : Options) =
    match options.SourceFile with
    | Some path ->
        try
            let colorScheme = ColorScheme.Script
            let logger = new Logger (colorScheme, Verbosity.Normal, options.LineNumber)
            LambdaInterpreter.StartOnFile (path = path, logger = logger)
        with
        | :? IOException
        | :? UnauthorizedAccessException as ex ->
            Logger.PrintError ex.Message
            exit <| int ExitCode.FileNotFound
    | None ->
        let colorScheme = ColorScheme.Interactive
        let logger = new Logger (colorScheme, Verbosity.Normal, false)
        LambdaInterpreter.StartInteractive (logger = logger)

let runInterpreter (interpreter : LambdaInterpreter) =
    using
        interpreter
        (fun interpreter ->
            interpreter.RunToEnd () |> Seq.iter ignore
            interpreter
        )

let exit = getExitCode >> int >> exit

Options.GetFromArgs ()
|> checkForError
|> tryPrintHelp
|> createInterpreter
|> runInterpreter
|> exit
