namespace Lambdy.Lib.Interfaces

open Lambdy.Lib.ParseTree
open Lambdy.Lib.IR
open Lambdy.Lib.LambdaTerm

type Interactive =
    | WaitingForInput
    | SyntaxHelp
    | Clear
    | ListBindings of (VariableName * IR) list
    | Output of InterpreterOutput

type Reduction =
    | StartedReducing of IR
    | DoneReducing of IR
    | CurrentTerm of LambdaTerm

type Transformation =
    | Substitution of VariableName * LambdaTerm
    | AlphaConversion of LambdaTerm * VariableName * VariableName
    | BetaReduction of LambdaTerm * LambdaTerm * VariableName * LambdaTerm

type Binding =
    | BoundValue of VariableName * LambdaTerm
    | NewBinding of VariableName * LambdaTerm
    | ResetBindings

type LogRecord =
    | Interactive of Interactive
    | Reduction of Reduction
    | Transformation of Transformation
    | Binding of Binding

/// An interface representing a class for managing logs.
[<Interface>]
type ILogger =

    /// Handle the given log record.
    abstract member Log : LogRecord -> unit
