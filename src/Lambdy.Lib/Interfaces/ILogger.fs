namespace Lambdy.Lib.Interfaces

open System.Threading.Tasks

open Lambdy.Lib.ParseTree
open Lambdy.Lib.IR
open Lambdy.Lib.LambdaTerm
open Lambdy.Lib.Interfaces.ILambdaInterpreter

module ILogger =

    type Minimal =
        | WaitingForInput
        | CurrentScope of bindings : (VariableName * IR * LambdaTerm) list
        | SyntaxHelp
        | InterpreterOutput of output : InterpreterOutput

    type Normal =
        | StartedReducing of term : IR
        | CurrentTerm of reductionCount : int * term : IR

    type Detailed =
        | Expansion of var : VariableName * value : IR
        | AlphaConversion of src : IR * var : VariableName * sub : VariableName
        | EtaReduction of src : IR * res : IR
        | BetaReduction of src : IR * body : IR * var : VariableName * sub : IR

    type Diagnostic =
        | CurrentlyReducing of term : IR
        | Substitution of var : VariableName * sub : IR
        | BoundValue of var : VariableName * value : IR
        | DelayedAlpha of term : IR * var : VariableName * sub : VariableName
        | NewBinding of var : VariableName * value : IR * expanded : LambdaTerm
        | WasReset

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
