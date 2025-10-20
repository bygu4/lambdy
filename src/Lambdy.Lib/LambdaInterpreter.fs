namespace Lambdy.Lib

open System
open System.IO

open FParsec

open Lambdy.Lib.ParseTree
open Lambdy.Lib.Parsers
open Lambdy.Lib.AST
open Lambdy.Lib.LambdaTerm
open Lambdy.Lib.Strategy
open Lambdy.Lib.Reducer
open Lambdy.Lib.Interfaces.IReducer
open Lambdy.Lib.Interfaces.ILambdaInterpreter
open Lambdy.Lib.Interfaces.ILogger

module LambdaInterpreter =

    /// Class representing the lambda term interpreter.
    type LambdaInterpreter
        private
        (
            stream : Stream,
            interactive : bool,
            ?strategy : Strategy,
            ?etaConversions : bool,
            ?reductionLimit : int,
            ?logger : ILogger
        )
        =
        let reader = new StreamReader (stream)

        let reducer =
            new Reducer (
                ?strategy = strategy,
                ?etaConversions = etaConversions,
                ?reductionLimit = reductionLimit,
                ?logger = logger
            )

        let mutable currentLine = 0
        let mutable syntaxError = false
        let mutable reductionFailed = false

        // ============= Creating an interpreter ==============================

        /// Create an interactive interpreter for the standard console input.
        /// Use `verbose` option to print logs to the console.
        static member StartInteractive
            (?strategy : Strategy, ?etaConversions : bool, ?reductionLimit : int, ?logger : ILogger)
            =
            new LambdaInterpreter (
                stream = Console.OpenStandardInput (),
                interactive = true,
                ?strategy = strategy,
                ?etaConversions = etaConversions,
                ?reductionLimit = reductionLimit,
                ?logger = logger
            )

        /// Create an interpreter to run on a source file at the given `path`.
        /// Use `verbose` option to print logs to the console.
        /// Use `lineNumber` option to add line number to the output.
        static member StartOnFile
            (path : string, ?strategy : Strategy, ?etaConversions : bool, ?reductionLimit : int, ?logger : ILogger)
            =
            new LambdaInterpreter (
                stream = File.OpenRead path,
                interactive = false,
                ?strategy = strategy,
                ?etaConversions = etaConversions,
                ?reductionLimit = reductionLimit,
                ?logger = logger
            )

        // ============= Properties ===========================================

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

        // ============= Running the interpreter ==============================

        /// Run the interpreter while possible and yield the interpretation results.
        member this.RunToEnd () : InterpreterOutput seq =
            seq {
                while this.CanRun do
                    let output = this.RunOnNextLineAsync () |> Async.RunSynchronously

                    match output with
                    | Some output ->
                        ILogger.TryLog (logger, Minimal <| InterpreterOutput output)
                        yield output
                    | None -> ()
            }

        /// Analyze the next line in the stream and get the interpretation result.
        member private _.RunOnNextLineAsync () =

            let logIfInteractive record =
                if interactive then
                    ILogger.TryLog (logger, record)

            /// Execute the given special interpreter `command`.
            let inline interpretCommand command =
                match command with
                | Reset ->
                    reducer.Reset ()
                    ILogger.TryLog (logger, Diagnostic WasReset)
                | Scope ->
                    let scope =
                        List.map (fun (var, value) -> var, value, toLambdaTerm true value) reducer.Scope

                    ILogger.TryLog (logger, Minimal <| CurrentScope scope)
                | Help -> ILogger.TryLog (logger, Minimal SyntaxHelp)
                | Clear -> ILogger.TryClearBuffer logger
                | Exit -> reader.Close ()

                None

            /// Interpret the given `primary` expression representation.
            let inline interpretExpression primary =
                match buildExpressionAST primary with
                | Binding (var, term) ->
                    reducer.AddBinding (var, term)
                    ILogger.TryLog (logger, Diagnostic <| NewBinding (var, term, toLambdaTerm true term))
                    None
                | Result term ->
                    ILogger.TryLog (logger, Normal <| StartedReducing term)

                    match reducer.Reduce term with
                    | ReductionResult.Success (normalForm, reductionCount) ->
                        let scope =
                            List.map (fun (var, value) -> var, toLambdaTerm true value) reducer.Scope

                        let alphaEquiv = findAlphaEquiv scope normalForm
                        let result = Success (normalForm, alphaEquiv, reductionCount)
                        Output (Line currentLine, result) |> Some
                    | ReductionResult.Failure errorMessage ->
                        reductionFailed <- true
                        Output (Line currentLine, Failure errorMessage) |> Some
                | Command command -> interpretCommand command
                | Empty -> None

            async {
                currentLine <- currentLine + 1
                logIfInteractive (Minimal WaitingForInput)
                let! line = reader.ReadLineAsync () |> Async.AwaitTask
                let parserResult = line |> run (expression interactive)

                return
                    match parserResult with
                    | ParserResult.Success (expr, _, _) -> interpretExpression expr
                    | ParserResult.Failure (msg, _, _) ->
                        syntaxError <- true
                        Output (Line currentLine, Failure msg) |> Some
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
