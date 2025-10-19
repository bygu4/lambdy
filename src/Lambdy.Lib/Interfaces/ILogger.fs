namespace Lambdy.Lib.Interfaces

open System.Threading.Tasks

open Lambdy.Lib.ParseTree
open Lambdy.Lib.IR
open Lambdy.Lib.Interfaces.ILambdaInterpreter

module ILogger =

    type Minimal =
        | WaitingForInput
        | CurrentScope of bindings : (VariableName * IR) list
        | SyntaxHelp
        | InterpreterOutput of output : InterpreterOutput

    type Normal =
        | StartedReducing of term : IR
        | CurrentTerm of term : IR
        | Expansion of var : VariableName * value : IR
        | AlphaConversion of src : IR * var : VariableName * sub : VariableName
        | EtaReduction of src : IR * res : IR
        | BetaReduction of src : IR * body : IR * var : VariableName * sub : IR

    type Detailed =
        | CurrentlyReducing of term : IR
        | NewBinding of var : VariableName * value : IR
        | WasReset

    type Diagnostic =
        | Substitution of var : VariableName * sub : IR
        | ValueBinding of var : VariableName * value : IR
        | DelayedAlpha of term : IR * var : VariableName * sub : VariableName

    type LogRecord =
        | Minimal of record : Minimal
        | Normal of record : Normal
        | Detailed of record : Detailed
        | Diagnostic of record : Diagnostic

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
