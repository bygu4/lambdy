namespace Lambdy.Bin

open System

/// Module containing utility regarding the console output color.
module ColorScheme =

    [<Literal>]
    let defaultSuccessColor = ConsoleColor.Green

    [<Literal>]
    let defaultErrorColor = ConsoleColor.Red

    type ColorScheme
        private
        (
            successColor : ConsoleColor,
            errorColor : ConsoleColor,
            textColor : ConsoleColor,
            accentColor : ConsoleColor,
            noteColor : ConsoleColor,
            diagnosticColor : ConsoleColor
        )
        =
        struct
            static member Script =
                new ColorScheme (
                    defaultSuccessColor,
                    defaultErrorColor,
                    ConsoleColor.White,
                    ConsoleColor.DarkYellow,
                    ConsoleColor.Gray,
                    ConsoleColor.DarkGray
                )

            static member Interactive =
                new ColorScheme (
                    defaultSuccessColor,
                    ConsoleColor.Yellow,
                    ConsoleColor.White,
                    ConsoleColor.DarkYellow,
                    ConsoleColor.Gray,
                    ConsoleColor.DarkGray
                )

            member _.SuccessColor = successColor
            member _.ErrorColor = errorColor
            member _.TextColor = textColor
            member _.AccentColor = accentColor
            member _.NoteColor = noteColor
            member _.DiagnosticColor = diagnosticColor
        end
