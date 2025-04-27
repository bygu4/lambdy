namespace LambdaInterpreter

/// Module containing utility regarding the exit code of the app.
module ExitCode =

    type ExitCode =

        /// The program execution was successful.
        | Success = 0

        /// An syntax error has occurred during source file interpretation.
        | SyntaxError = 1

        /// The given source file was not found.
        | FileNotFound = 2

    /// Get the exit code of the program according to the state of the given `interpreter`.
    let getExitCode (interpreter: Interpreter) =
        if interpreter.IsInteractive || not interpreter.SyntaxError then ExitCode.Success
        else ExitCode.SyntaxError
