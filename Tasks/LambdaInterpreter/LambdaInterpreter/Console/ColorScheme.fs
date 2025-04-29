namespace LambdaInterpreter

open System

/// Module containing utility regarding the console output color.
module ColorScheme =

    /// Color of messages signaling of success.
    type SuccessColor = Color of ConsoleColor

    /// Color of messages signaling of error.
    type ErrorColor = Color of ConsoleColor

    /// Color scheme of the console application.
    type ColorScheme = SuccessColor * ErrorColor

    /// Get the color scheme of the app according to the state of the given `interpreter`.
    let getColorScheme (interpreter: Interpreter) =
        if interpreter.IsInteractive then Color ConsoleColor.Green, Color ConsoleColor.Yellow
        else Color ConsoleColor.Green, Color ConsoleColor.Red
