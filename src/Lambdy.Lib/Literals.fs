namespace Lambdy.Lib

/// Module dealing with literal definitions for the parser.
module Literals =

    /// A regex pattern representing the variable name.
    [<Literal>]
    let VariablePattern = @"[a-zA-Z][a-zA-Z0-9_']*"

    /// Keyword for variable declaration.
    [<Literal>]
    let DeclarationKeyword = "let"

    /// Keyword for resetting defined variables.
    [<Literal>]
    let ResetKeyword = "reset"

    /// Keyword for displaying bound variables of current environment.
    [<Literal>]
    let ScopeKeyword = "scope"

    /// Keyword for displaying help info.
    [<Literal>]
    let HelpKeyword = "help"

    /// Keyword for clearing the console buffer.
    [<Literal>]
    let ClearKeyword = "clear"

    /// Keyword for exiting the interpreter.
    [<Literal>]
    let ExitKeyword = "exit"

    [<Literal>]
    let SeparatorChar = '.'

    [<Literal>]
    let CommandChar = '!'

    [<Literal>]
    let CommentChar = '#'

    let lambdaChars = [| 'λ' ; '\\' |]

    /// The list of defined keywords.
    let reservedNames = [| DeclarationKeyword |]

    /// Whether the given `str` is reserved as a keyword.
    let isReserved str = Array.contains str reservedNames
