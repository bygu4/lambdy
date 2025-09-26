namespace Lambdy.Bin

open System

open Lambdy.Lib.Literals

/// Module for displaying command line help.
module Help =

    [<Literal>]
    let Indent = "    "

    /// A pointer indicating the start of input for the interpreter.
    [<Literal>]
    let InputPointer = ">>> "

    /// Message to print when failed to interpret the given command line arguments.
    let invalidArgsMessage =
        $"Invalid command line arguments provided. For more info use '{Array.last Options.HelpArgs}' option."

    /// Print header with general info about the app.
    let printHeader () =
        printfn
            "
Lambdy
-------------------------------------------
An interactive lambda calculus interpreter."

    /// Print help suggestion with a corresponding command to the console.
    let printHelpSuggestion () =
        printfn
            $"
Type '{HelpKeyword}' for more info.
"

    /// Print command line usage help to the console.
    let printUsageHelp () =
        printfn
            "
It can either be run interactively using the standard input,
or on a specified source file.

Usage: LambdaInterpreter [path-to-file] [options]"

    /// Print command line options help to the console.
    let printOptionsHelp () =
        printfn
            $"
Options:
{Indent}{String.Join ('|', Options.HelpArgs)}\t\t Display help and exit
{Indent}{String.Join ('|', Options.VerboseArgs)}\t Print detailed interpretation info
{Indent}{String.Join ('|', Options.LineNumberArgs)}\t Print line number with output"

    /// Print syntax help to the standard output.
    let printSyntaxHelp () =
        printfn
            $"
Syntax:
{Indent}variable\t\t {VariablePattern}
{Indent}term\t\t {{variable}}|{{abstraction}}|{{application}}|({{term}})
{Indent}application\t\t {{term}} {{term}}
{Indent}abstraction\t\t \\{{variables}}.{{term}}
{Indent}definition\t\t {DeclarationKeyword} {{variable}} = {{term}}

Examples:
{Indent}{InputPointer}{DeclarationKeyword} S = \\x y z.x z (y z)
{Indent}{InputPointer}{DeclarationKeyword} K = \\x y.x
{Indent}{InputPointer}S K K
{Indent}\\z.z

Commands:
{Indent}{ResetKeyword}\t\t Reset defined variables
{Indent}{DisplayKeyword}\t\t Display definitions in order of addition
{Indent}{HelpKeyword}\t\t Display help
{Indent}{ClearKeyword}\t\t Clear console buffer
{Indent}{ExitKeyword}\t\t Stop the execution and exit
"
