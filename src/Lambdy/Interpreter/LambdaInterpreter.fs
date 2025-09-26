namespace Lambdy.Interpreter

open System
open System.IO
open FParsec

open Lambdy.Syntax.ParseTree
open Lambdy.Syntax.Parsers
open Lambdy.Syntax.LambdaTerm
open Lambdy.Syntax.AST
open Lambdy.Interfaces

/// Class representing the lambda term interpreter.
type LambdaInterpreter private (stream : Stream, interactive : bool, logger : ILogger) =
    let reader = new StreamReader (stream)
    let reducer = new Reducer (logger, 1000000)

    let mutable currentLine = 0
    let mutable syntaxError = false
    let mutable maxDepthExceeded = false

    /// Create an interactive interpreter for the standard console input.
    /// Use `verbose` option to print logs to the console.
    static member StartInteractive (logger : ILogger) : LambdaInterpreter =
        new LambdaInterpreter (Console.OpenStandardInput (), true, logger)

    /// Create an interpreter to run on a source file at the given `path`.
    /// Use `verbose` option to print logs to the console.
    /// Use `lineNumber` option to add line number to the output.
    static member StartOnFile (path : string, logger : ILogger) : LambdaInterpreter =
        new LambdaInterpreter (File.OpenRead path, false, logger)

    /// Whether the interpreter is being run in console.
    member _.IsInteractive : bool = interactive

    /// Whether the current state of the interpreter supports running.
    member _.CanRun : bool =
        if interactive then
            stream.CanRead
        else
            stream.CanRead && not reader.EndOfStream

    /// Whether a syntax error occurred during the interpretation.
    member _.SyntaxError : bool = syntaxError

    /// Whether max allowed depth of recursion exceeded during the term reduction.
    member _.MaxDepthExceeded : bool = maxDepthExceeded

    /// Run the interpreter while possible and yield the interpretation results.
    member this.RunToEnd () : InterpreterOutput seq =
        seq {
            while this.CanRun do
                let result = this.RunOnNextLineAsync () |> Async.RunSynchronously

                match result with
                | Some output -> yield output
                | None -> ()
        }

    /// Analyze the next line in the stream and get the interpretation result.
    member private _.RunOnNextLineAsync () : Async<InterpreterOutput option> =

        /// Execute the given special interpreter `command`.
        let runCommand command =
            match command with
            | Reset -> reducer.ResetBindings ()
            | Display ->
                if interactive then
                    logger.Log <| Interactive (ListBindings reducer.Bindings)
            | Help ->
                if interactive then
                    logger.Log <| Interactive SyntaxHelp
            | Clear ->
                if interactive then
                    logger.Log <| Interactive Clear
            | Exit -> reader.Close ()

            None

        /// Interpret the given `primary` expression representation.
        let interpretExpression primary =
            match buildExpressionAST primary with
            | Definition (var, term) ->
                reducer.AddBinding (var, term)
                None
            | Result term ->
                match reducer.Reduce term with
                | Result.Ok _ as result -> (Line currentLine, result) |> Some
                | _ as err ->
                    maxDepthExceeded <- true
                    (Line currentLine, err) |> Some
            | Command command -> runCommand command
            | Empty -> None

        /// Log a request for input when running an interactive interpreter.
        let requestInput () =
            if interactive then
                logger.Log <| Interactive WaitingForInput

        async {
            currentLine <- currentLine + 1
            requestInput ()
            let! line = reader.ReadLineAsync () |> Async.AwaitTask
            let parserResult = line |> run expression

            return
                match parserResult with
                | Success (expr, _, _) -> interpretExpression expr
                | Failure (msg, _, _) ->
                    syntaxError <- true
                    (Line currentLine, Result.Error msg) |> Some
        }

    /// Free resources used by the interpreter.
    member _.Dispose () = reader.Dispose ()

    interface IDisposable with
        member _.Dispose () = reader.Dispose ()

    interface ILambdaInterpreter with
        member this.IsInteractive = this.IsInteractive
        member this.CanRun = this.CanRun
        member this.SyntaxError = this.SyntaxError
        member this.MaxDepthExceeded = this.MaxDepthExceeded
        member this.RunToEnd () = this.RunToEnd ()
