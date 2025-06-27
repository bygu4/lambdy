namespace Lambdy.Interpreter.Logger

/// An interface representing a class for managing logs.
[<Interface>]
type ILogger =

    /// Handle the given log record.
    abstract member Log : LogRecord -> unit
