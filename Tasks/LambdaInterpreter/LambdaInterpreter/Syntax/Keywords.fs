namespace LambdaInterpreter

/// Module dealing with keyword definitions.
module Keywords =

    /// A regex pattern representing the variable name.
    [<Literal>]
    let VariablePattern = @"[a-zA-Z][a-zA-Z0-9_]*"

    /// Keyword for variable declaration.
    [<Literal>]
    let DeclarationKeyword = "let"

    /// Keyword for clearing the list of defined variables.
    [<Literal>]
    let ClearKeyword = "clear"

    /// Keyword for displaying help info.
    [<Literal>]
    let HelpKeyword = "help"

    /// Keyword for exiting the interpreter.
    [<Literal>]
    let ExitKeyword = "exit"

    /// The list of defined keywords.
    let keywords = [
        DeclarationKeyword;
        ClearKeyword;
        HelpKeyword;
        ExitKeyword;
    ]

    /// Whether the given `str` is reserved as a keyword.
    let isKeyword str = List.contains str keywords
