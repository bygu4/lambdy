namespace LambdaInterpreter.Syntax

/// Module dealing with keyword definitions.
module Keywords =

    /// A regex pattern representing the variable name.
    [<Literal>]
    let VariablePattern = @"[a-zA-Z][a-zA-Z0-9_]*"

    /// Keyword for variable declaration.
    [<Literal>]
    let DeclarationKeyword = "let"

    /// Keyword for resetting defined variables.
    [<Literal>]
    let ResetKeyword = "reset"

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
    let keywords = [
        DeclarationKeyword;
        ResetKeyword;
        HelpKeyword;
        ClearKeyword;
        ExitKeyword;
    ]

    /// Whether the given `str` is reserved as a keyword.
    let isKeyword str = List.contains str keywords
