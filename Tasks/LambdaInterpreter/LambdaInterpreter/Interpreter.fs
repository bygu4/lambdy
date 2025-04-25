namespace LambdaInterpreter

open System
open System.Text
open FParsec

open AST
open Parser
open Reduction

/// Module dealing with lambda term interpretation.
module Interpreter =

    /// Run the interpreter to the end of the given `stream`.
    let runInterpreter (interactive: bool) (stream: CharStream<_>) =
        let mutable vars = new Map<Variable, LambdaTerm> ([])
        seq {
            while not stream.IsEndOfStream do
                if interactive then printf "-> "
                let reply = expression stream
                if reply.Status = Ok then
                    let expr = reply.Result |> buildAST_Expression
                    match expr with
                    | Definition (var, term) ->
                        vars <- vars.Add (var, substituteMany term vars)
                    | Result term ->
                        let result = substituteMany term vars |> reduce |> toString
                        if interactive then printfn "%s" result
                        yield Result.Ok result
                    | None -> ()
                else
                    let message = reply.Error.Head.ToString ()
                    if interactive then printfn "%s" message
                    yield Result.Error message
        }

    /// Run the interpreter on a source file at the given `path`.
    let runInterpreterOnFile (path: string) =
        use stream = new CharStream<_> (path, Encoding.UTF8)
        runInterpreter false stream

    /// Interactively run the interpreter in the console.
    let runInterpreterInConsole () =
        use inputStream = Console.OpenStandardInput ()
        let stream = new CharStream<_> (inputStream, Encoding.UTF8)
        runInterpreter true stream
