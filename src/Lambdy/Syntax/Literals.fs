namespace Lambdy.Syntax

/// Module dealing with literal definitions for the parser.
module Literals =

    /// A regex pattern representing the variable name.
    [<Literal>]
    let VariablePattern = @"[a-zA-Z][a-zA-Z0-9_]*"

    /// Keyword for variable declaration.
    [<Literal>]
    let DeclarationKeyword = "let"

    /// Keyword for resetting defined variables.
    [<Literal>]
    let ResetKeyword = "reset"

    /// Keyword for displaying defined variables.
    [<Literal>]
    let DisplayKeyword = "display"

    /// Keyword for displaying help info.
    [<Literal>]
    let HelpKeyword = "help"

    /// Keyword for clearing the console buffer.
    [<Literal>]
    let ClearKeyword = "clear"

    /// Keyword for exiting the interpreter.
    [<Literal>]
    let ExitKeyword = "exit"

    /// The list of defined keywords.
    let keywords =
        [
            DeclarationKeyword
            ResetKeyword
            DisplayKeyword
            HelpKeyword
            ClearKeyword
            ExitKeyword
        ]

    /// Whether the given `str` is reserved as a keyword.
    let isKeyword str = List.contains str keywords
