namespace Lambdy.Lib.Interfaces

open Lambdy.Lib.LambdaTerm

module ILambdaInterpreter =

    /// The number of line of the source given to the interpreter.
    type LineNumber = | Line of int

    type InterpretationResult =
        | Success of normalForm : LambdaTerm * alphaEquivalent : LambdaTerm option * reductionCount : int
        | Failure of errorMessage : string

    /// The output of the interpreter.
    type InterpreterOutput = | Output of line : LineNumber * result : InterpretationResult

    /// An interface representing the lambda calculus interpreter.
    [<Interface>]
    type ILambdaInterpreter =

        /// Whether the interpreter is being run in console.
        abstract member IsInteractive : bool

        /// Whether the current state of the interpreter supports running.
        abstract member CanRun : bool

        /// Whether a syntax error occurred during the interpretation.
        abstract member SyntaxError : bool

        /// Whether max allowed depth of recursion exceeded during the term reduction.
        abstract member ReductionFailed : bool

        /// Run the interpreter while possible and yield the interpretation results.
        abstract member RunToEnd : unit -> InterpreterOutput seq
