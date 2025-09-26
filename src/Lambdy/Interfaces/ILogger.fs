namespace Lambdy.Interfaces

open Lambdy.Syntax.ParseTree
open Lambdy.Syntax.LambdaTerm

type Interactive =
    | WaitingForInput
    | SyntaxHelp
    | Clear
    | ListBindings of (VariableName * LambdaTerm) list
    | Output of InterpreterOutput

type Reduction =
    | StartedReducing of LambdaTerm
    | DoneReducing of LambdaTerm
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
