namespace LambdaInterpreter.Console

open System

/// Struct for managing command line options.
type Options private (sourceFile: string option, help: bool, verbose: bool, lineNumber: bool, error: bool) =
    struct
        /// Command line arguments for displaying help.
        static member HelpArgs: string array = [| "-h"; "--help" |]

        /// Command line arguments for printing detailed output to the console.
        static member VerboseArgs: string array = [| "-v"; "--verbose" |]

        /// Command line arguments for printing source file line number with output.
        static member LineNumberArgs: string array = [| "-n"; "--line-number" |]

        /// A path of the source file to run the interpreter on.
        member _.SourceFile: string option = sourceFile

        /// Whether to display command line help.
        member _.Help: bool = help

        /// Whether to print detailed output to the console.
        member _.Verbose: bool = verbose

        /// Whether to print source file line number with output.
        member _.LineNumber: bool = lineNumber

        /// Whether the given command line arguments are invalid.
        member _.Error: bool = error

        /// Get options from command line arguments.
        static member GetFromArgs() : Options =

            /// Get difference of `source` with `target` with a value indicating their intersection.
            let removeMany source target =
                let result = target |> Set |> Set.difference source
                result, result <> source

            let args = Environment.GetCommandLineArgs() |> Array.removeAt 0 |> Set
            let args, help = removeMany args Options.HelpArgs
            let args, verbose = removeMany args Options.VerboseArgs
            let args, lineNumber = removeMany args Options.LineNumberArgs
            let sourceFile = if args.Count = 1 then Some(Set.minElement args) else None
            let error = args.Count > 1
            new Options(sourceFile, help, verbose, lineNumber, error)
    end
