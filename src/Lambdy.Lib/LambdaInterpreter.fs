namespace Lambdy.Lib

open System
open System.IO
open FParsec

open Lambdy.Lib.ParseTree
open Lambdy.Lib.Parsers
open Lambdy.Lib.AST
open Lambdy.Lib.Reducer
open Lambdy.Lib.Interfaces

module LambdaInterpreter =

    /// Class representing the lambda term interpreter.
    type LambdaInterpreter private (stream : Stream, interactive : bool, ?logger : ILogger) =
        let reader = new StreamReader (stream)
        let reducer = new Reducer (1000000, ?logger = logger)

        let mutable currentLine = 0
        let mutable syntaxError = false
        let mutable reductionFailed = false

        let logIfInteractive record =
            if interactive then
                ILogger.TryLog (logger, record)

        /// Create an interactive interpreter for the standard console input.
        /// Use `verbose` option to print logs to the console.
        static member StartInteractive (?logger : ILogger) =
            new LambdaInterpreter (Console.OpenStandardInput (), true, ?logger = logger)

        /// Create an interpreter to run on a source file at the given `path`.
        /// Use `verbose` option to print logs to the console.
        /// Use `lineNumber` option to add line number to the output.
        static member StartOnFile (path : string, ?logger : ILogger) =
            new LambdaInterpreter (File.OpenRead path, false, ?logger = logger)

        /// Whether the interpreter is being run in console.
        member _.IsInteractive = interactive

        /// Whether the current state of the interpreter supports running.
        member _.CanRun =
            if interactive then
                stream.CanRead
            else
                stream.CanRead && not reader.EndOfStream

        /// Whether a syntax error occurred during the interpretation.
        member _.SyntaxError = syntaxError

        /// Whether max allowed depth of recursion exceeded during the term reduction.
        member _.ReductionFailed = reductionFailed

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
        member private _.RunOnNextLineAsync () =

            /// Execute the given special interpreter `command`.
            let runCommand command =
                match command with
                | Reset -> reducer.ResetBindings ()
                | Scope -> logIfInteractive (Minimal <| CurrentScope reducer.Bindings)
                | Help -> logIfInteractive (Minimal SyntaxHelp)
                | Clear -> logIfInteractive (Minimal ClearBuffer)
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
                    | Result.Ok _ as result -> Output (Line currentLine, result) |> Some
                    | _ as err ->
                        reductionFailed <- true
                        Output (Line currentLine, err) |> Some
                | Command command -> runCommand command
                | Empty -> None

            async {
                currentLine <- currentLine + 1
                logIfInteractive (Minimal WaitingForInput)
                let! line = reader.ReadLineAsync () |> Async.AwaitTask
                let parserResult = line |> run expression

                return
                    match parserResult with
                    | Success (expr, _, _) -> interpretExpression expr
                    | Failure (msg, _, _) ->
                        syntaxError <- true
                        Output (Line currentLine, Result.Error msg) |> Some
            }

        /// Free resources used by the interpreter.
        member _.Dispose () = reader.Dispose ()

        interface IDisposable with
            member _.Dispose () = reader.Dispose ()

        interface ILambdaInterpreter with
            member this.IsInteractive = this.IsInteractive
            member this.CanRun = this.CanRun
            member this.SyntaxError = this.SyntaxError
            member this.ReductionFailed = this.ReductionFailed
            member this.RunToEnd () = this.RunToEnd ()
