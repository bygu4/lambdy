namespace Lambdy.Lib.Interfaces

open System.Threading.Tasks

open Lambdy.Lib.ParseTree
open Lambdy.Lib.IR
open Lambdy.Lib.Interfaces.ILambdaInterpreter

module ILogger =

    type Minimal =
        | WaitingForInput
        | CurrentScope of (VariableName * IR) list
        | SyntaxHelp
        | InterpreterOutput of InterpreterOutput

    type Normal =
        | StartedReducing of IR
        | CurrentTerm of IR
        | Unbinding of VariableName * IR
        | AlphaConversion of IR * VariableName * VariableName
        | EtaConversion of IR * IR
        | BetaReduction of IR * IR * VariableName * IR

    type Detailed =
        | CurrentlyReducing of IR
        | NewBinding of VariableName * IR
        | WasReset

    type Diagnostic =
        | Substitution of VariableName * IR
        | BoundValue of VariableName * IR
        | DelayedConversion of IR * VariableName * VariableName

    type LogRecord =
        | Minimal of Minimal
        | Normal of Normal
        | Detailed of Detailed
        | Diagnostic of Diagnostic

    /// An interface representing a class for managing logs.
    [<Interface>]
    type ILogger =

        /// Handle the given log record.
        abstract member LogAsync : LogRecord -> Task

        abstract member ClearBuffer : unit -> unit

        static member TryLog (logger : ILogger option, record : LogRecord) =
            match logger with
            | Some logger -> logger.LogAsync record |> ignore
            | None -> ()

        static member TryClearBuffer (logger : ILogger option) =
            match logger with
            | Some logger -> logger.ClearBuffer ()
            | None -> ()
