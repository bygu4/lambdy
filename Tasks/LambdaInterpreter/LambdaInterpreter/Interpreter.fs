namespace LambdaInterpreter

open System
open System.IO
open FParsec

open Keywords
open AST
open Parser
open Reduction

/// Class representing the lambda term interpreter.
type Interpreter private (stream: Stream, interactive: bool) =
    let reader = new StreamReader (stream)
    let mutable vars = new Map<Variable, LambdaTerm> ([])
    let mutable error = false

    /// Print help info to the standard output.
    static member PrintHelp () = printfn $"
Syntax:
    variable\t\t {VariablePattern}
    term\t\t {{variable}}|{{abstraction}}|{{application}}|({{term}})
    application\t\t {{term}} {{term}}
    abstraction\t\t \\{{variables}}.{{term}}
    definition\t\t {DeclarationKeyword} {{variable}} = {{term}}

Examples:
    -> {DeclarationKeyword} S = \\x y z.x z (y z)
    -> {DeclarationKeyword} K = \\x y.x
    -> S K K
    \\z.z

Commands:
    {ResetKeyword}\t\t Reset defined variables
    {HelpKeyword}\t\t Display help
    {ClearKeyword}\t\t Clear console buffer
    {ExitKeyword}\t\t Stop the execution and exit
"

    /// Print a pointer indicating the start of input when running the interactive interpreter.
    static member PrintInputPointer () =
        printf "-> "

    /// Create an interactive interpreter for the standard console input.
    static member StartInteractive (): Interpreter =
        new Interpreter (Console.OpenStandardInput (), true)

    /// Create an interpreter to run on source file at the given `path`.
    static member StartOnFile (path: string): Interpreter =
        new Interpreter (File.OpenRead path, false)

    /// Whether the interpreter is being run in console.
    member _.IsInteractive: bool = interactive

    /// Whether the current state of the interpreter supports running.
    member _.CanRun: bool = if interactive then stream.CanRead
                            else stream.CanRead && not reader.EndOfStream

    /// Whether a syntax error occurred during the interpretation.
    member _.SyntaxError: bool = error

    /// Analyze the next line in the stream and get the interpretation result.
    member _.RunOnNextLineAsync (): Async<Result<string, string>> =

        /// Execute the given special interpreter `command`.
        let runCommand (command: SpecialCommand) =
            match command with
            | Reset -> vars <- new Map<Variable, LambdaTerm> ([])
            | Help -> if interactive then Interpreter.PrintHelp ()
            | Clear -> if interactive then Console.Clear ()
            | Exit -> reader.Close ()
            Result.Ok String.Empty

        /// Interpret the given `primary` expression representation.
        let interpretExpression (primary: Primary.Expression) =
            match buildAST_Expression primary with
            | Definition (var, term) ->
                vars <- vars.Add (var, substituteMany term vars)
                Result.Ok String.Empty
            | Result term ->
                let result = substituteMany term vars |> reduce |> toString
                Result.Ok result
            | Command command -> runCommand command
            | Empty -> Result.Ok String.Empty

        if interactive then Interpreter.PrintInputPointer ()
        async {
            let! line = reader.ReadLineAsync () |> Async.AwaitTask
            let parserResult = line |> run expression
            return
                match parserResult with
                | Success (expr, _, _) -> interpretExpression expr
                | Failure (msg, _, _) ->
                    error <- true
                    Result.Error msg
        }

    /// Run the interpreter while possible and yield result for each of the lines.
    member self.RunToEnd (): Async<Result<string, string>> seq =
        seq {
            while self.CanRun do
                yield self.RunOnNextLineAsync ()
        }

    interface IDisposable with

        /// Free resources used by the interpreter.
        member _.Dispose () = reader.Dispose ()

    /// Free resources used by the interpreter.
    member _.Dispose () = reader.Dispose ()
