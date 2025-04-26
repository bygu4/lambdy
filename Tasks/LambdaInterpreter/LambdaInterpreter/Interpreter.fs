namespace LambdaInterpreter

open System
open System.IO
open FParsec

open AST
open Parser
open Reduction

/// Class representing the lambda term interpreter.
/// Create a parser instance to be run on the given `stream`.
/// Use `interactive` parameter if the interpreter is meant to be run in console.
type Interpreter (stream: Stream, ?interactive: bool) =
    let reader = new StreamReader (stream)
    let mutable vars = new Map<Variable, LambdaTerm> ([])
    let mutable error = false

    /// Create interpreter for the standard console input.
    new () = new Interpreter (Console.OpenStandardInput (), true)

    /// Create interpreter for the source file at the given `path`.
    new (path: string) = new Interpreter (File.OpenRead path)

    /// Whether the interpreter is interactive.
    member _.IsInteractive: bool = defaultArg interactive false

    /// Whether the end of stream was reached by the interpreter.
    member _.EndOfStream: bool = reader.EndOfStream

    /// Whether an error occurred during the interpretation.
    member _.HadError: bool = error

    /// Analyze the next line in the stream and get the interpretation result.
    member _.RunOnNextLineAsync (): Async<Result<string, string>> =

        /// Interpret the given `primary` expression representation.
        let interpretExpression primary =
            match buildAST_Expression primary with
            | Definition (var, term) ->
                vars <- vars.Add (var, substituteMany term vars)
                Result.Ok String.Empty
            | Result term ->
                let result = substituteMany term vars |> reduce |> toString
                Result.Ok result
            | Empty -> Result.Ok String.Empty

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

    /// Run the interpreter until the end of stream.
    /// Yield interpretation result for each of the lines.
    member self.RunToEnd (): Result<string, string> seq =
        seq {
            while not self.EndOfStream do
                yield Async.RunSynchronously <| self.RunOnNextLineAsync ()
        }

    interface IDisposable with

        /// Free resources used by the interpreter.
        member _.Dispose () = reader.Dispose ()
