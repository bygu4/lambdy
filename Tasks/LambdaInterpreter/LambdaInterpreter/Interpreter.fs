namespace LambdaInterpreter

open System
open System.IO
open FParsec

open Keywords
open AST
open Parser

/// Class representing the lambda term interpreter.
type Interpreter private (stream: Stream, interactive: bool, ?verbose: bool, ?lineNumber: bool) =
    let reader = new StreamReader (stream)
    let reducer = new Reducer (?verbose=verbose)
    let lineNumber = defaultArg lineNumber false

    let mutable currentLine = 0
    let mutable syntaxError = false
    let mutable stackOverflow = false

    /// Print a pointer indicating the start of input when running an interactive interpreter.
    let tryPrintInputPointer () =
        if interactive then printf ">>> "

    /// Add current line number to the given string when running on a source file.
    let tryAddCurrentLine str =
        if lineNumber then $"[Line {currentLine}] {str}" else str

    /// Print help info to the standard output.
    static member PrintHelp () = printfn $"
Syntax:
    variable\t\t {VariablePattern}
    term\t\t {{variable}}|{{abstraction}}|{{application}}|({{term}})
    application\t\t {{term}} {{term}}
    abstraction\t\t \\{{variables}}.{{term}}
    definition\t\t {DeclarationKeyword} {{variable}} = {{term}}

Examples:
    >>> {DeclarationKeyword} S = \\x y z.x z (y z)
    >>> {DeclarationKeyword} K = \\x y.x
    >>> S K K
    \\z.z

Commands:
    {ResetKeyword}\t\t Reset defined variables
    {HelpKeyword}\t\t Display help
    {ClearKeyword}\t\t Clear console buffer
    {ExitKeyword}\t\t Stop the execution and exit
"

    /// Create an interactive interpreter for the standard console input.
    /// Use `verbose` option to print logs to the console.
    static member StartInteractive (?verbose: bool): Interpreter =
        new Interpreter (Console.OpenStandardInput (), true, ?verbose=verbose)

    /// Create an interpreter to run on a source file at the given `path`.
    /// Use `verbose` option to print logs to the console.
    /// Use `lineNumber` option to add line number to the output.
    static member StartOnFile (path: string, ?verbose: bool, ?lineNumber: bool): Interpreter =
        new Interpreter (File.OpenRead path, false, ?verbose=verbose, ?lineNumber=lineNumber)

    /// Whether the interpreter is being run in console.
    member _.IsInteractive: bool = interactive

    /// Whether the current state of the interpreter supports running.
    member _.CanRun: bool = if interactive then stream.CanRead
                            else stream.CanRead && not reader.EndOfStream

    /// Whether a syntax error occurred during the interpretation.
    member _.SyntaxError: bool = syntaxError

    /// Whether a stack overflow occurred during the term reduction.
    member _.StackOverflow: bool = stackOverflow

    /// Run the interpreter while possible and yield the interpretation results.
    member self.RunToEnd (): Result<string, string> seq =
        seq {
            while self.CanRun do
                let result = self.RunOnNextLineAsync () |> Async.RunSynchronously
                match result with
                | Some output -> yield output
                | None -> ()
        }

    /// Analyze the next line in the stream and get the interpretation result.
    member private _.RunOnNextLineAsync (): Async<Result<string, string> option> =

        /// Execute the given special interpreter `command`.
        let runCommand (command: SpecialCommand) =
            match command with
            | Reset -> reducer.Reset ()
            | Help -> if interactive then Interpreter.PrintHelp ()
            | Clear -> if interactive then Console.Clear ()
            | Exit -> reader.Close ()
            None

        /// Interpret the given `primary` expression representation.
        let interpretExpression (primary: Primary.Expression) =
            match buildAST_Expression primary with
            | Definition (var, term) ->
                reducer.AddDefinition (var, term)
                None
            | Result term ->
                try reducer.Reduce term
                    |> toString
                    |> tryAddCurrentLine
                    |> Result.Ok
                    |> Some
                with :? StackOverflowException as ex ->
                    stackOverflow <- true
                    ex.Message |> tryAddCurrentLine |> Result.Error |> Some
            | Command command -> runCommand command
            | Empty -> None

        async {
            currentLine <- currentLine + 1
            tryPrintInputPointer ()
            let! line = reader.ReadLineAsync () |> Async.AwaitTask
            let parserResult = line |> run expression
            return
                match parserResult with
                | Success (expr, _, _) -> interpretExpression expr
                | Failure (msg, _, _) ->
                    syntaxError <- true
                    msg |> tryAddCurrentLine |> Result.Error |> Some
        }

    interface IDisposable with

        /// Free resources used by the interpreter.
        member _.Dispose () = reader.Dispose ()

    /// Free resources used by the interpreter.
    member _.Dispose () = reader.Dispose ()
