namespace LambdaInterpreter.Console

open LambdaInterpreter

/// Module containing utility regarding the exit code of the app.
module ExitCode =

    /// Enum representing the exit code of the app.
    type ExitCode =

        /// The program execution was successful.
        | Success = 0

        /// The given command line arguments are invalid.
        | InvalidArguments = 1

        /// The given source file was not found.
        | FileNotFound = 2

        /// A syntax error occurred during source file interpretation.
        | SyntaxError = 3

        /// A stack overflow occurred during the source file term reduction.
        | StackOverflow = 4

    /// Get the exit code of the program according to the state of the given `interpreter`.
    let getExitCode (interpreter: Interpreter) =
        if interpreter.IsInteractive then ExitCode.Success else
        if interpreter.SyntaxError then ExitCode.SyntaxError else
        if interpreter.StackOverflow then ExitCode.StackOverflow else
        ExitCode.Success
