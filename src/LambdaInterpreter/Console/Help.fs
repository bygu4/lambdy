namespace LambdaInterpreter.Console

open System
open LambdaInterpreter.Syntax

/// Module for displaying command line help.
module Help =

    /// Message to print when failed to interpret the given command line arguments.
    let invalidArgsMessage =
        $"Invalid command line arguments provided. For more info use '{Array.last Options.HelpArgs}' option."

    /// Print header with general info about the app.
    let printHeader () =
        printfn "
Lambda Interpreter
------------------
An interactive lambda term interpreter."

    /// Print help suggestion with a corresponding command to the console.
    let printHelpSuggestion () =
        printfn $"
Type '{Keywords.HelpKeyword}' for more info.
"

    /// Print command line usage help to the console.
    let printUsageHelp () =
        printfn "
It can either be run interactively using the standard input,
or on a specified source file.

Usage: LambdaInterpreter [path-to-file] [options]"

    /// Print command line options help to the console.
    let printOptionsHelp () =
        printfn $"
Options:
    {String.Join ('|', Options.HelpArgs)}\t\t Display help and exit
    {String.Join ('|', Options.VerboseArgs)}\t Print detailed interpretation info
    {String.Join ('|', Options.LineNumberArgs)}\t Print line number with output"

    /// Print syntax help to the console.
    let printSyntaxHelp () = Help.printSyntaxHelp ()
