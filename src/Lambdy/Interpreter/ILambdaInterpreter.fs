namespace Lambdy.Interpreter

/// The number of line of the source given to the interpreter.
type LineNumber = | Line of int

/// The result of the interpretation.
type InterpretationResult = Result<string, string>

/// The output of the interpreter.
type InterpreterOutput = | Output of LineNumber * InterpretationResult

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
    abstract member MaxDepthExceeded : bool

    /// Run the interpreter while possible and yield the interpretation results.
    abstract member RunToEnd : unit -> InterpreterOutput seq
