namespace LambdaInterpreter

open FParsec
open Primary

/// Module dealing with the source text parsing.
module Parser =

    let variablePattern = @"[a-zA-Z][a-zA-Z0-9_]*"
    let declarationKeyword = "let"

    let keywords = [declarationKeyword]
    let isKeyword str = List.contains str keywords

    /// Accept 0 or more whitespaces.
    let whitespaceOpt = spaces

    /// Accept 1 or more whitespaces.
    let whitespace = spaces1

    /// Accept optional whitespace on the left of the given `parser`.
    let ( ?< ) parser = whitespaceOpt >>. parser

    /// Accept optional whitespace on the right of the given `parser`.
    let ( ?> ) parser = parser .>> whitespaceOpt

    /// Accept whitespace on the left of the given `parser`.
    let ( !< ) parser = whitespace >>. parser

    /// Accept whitespace on the right of the given `parser`.
    let ( !> ) parser = parser .>> whitespace

    /// Accept optional whitespace followed by end of the input.
    let inputEnd = (?<)eof

    /// Modifies the given `reply` to check that its result is not a keyword.
    let exceptKeyword (reply: Reply<string>) =
        if reply.Status = ReplyStatus.Ok && isKeyword reply.Result then
            Reply (
                ReplyStatus.Error,
                ErrorMessage.Unexpected $"\"{reply.Result}\" is reserved as a keyword" |> ErrorMessageList
            )
        else reply

    /// Accept the variable name.
    let variable: Parser<Variable, unit> = regex variablePattern >> exceptKeyword |>> Name

    /// Accept one or more of variable names.
    let variables = sepBy1 variable whitespace

    /// Accept a primary lambda term representation.
    let term, termRef = createParserForwardedToRef ()

    /// Accept an operand in lambda term application.
    let operand =
        between ((?>)(pchar '(')) ((?<)(pchar ')')) term |>> Brackets
        <|> (variable |>> Variable)

    /// Accept an optional lambda term application.
    let applicationOpt, applicationOptRef = createParserForwardedToRef ()

    applicationOptRef.Value <-
        attempt (!<operand .>>. applicationOpt) |>> Apply
        <|> preturn ApplicationOpt.Epsilon

    /// Accept a lambda abstraction.
    let abstraction = between (pchar '\\') (pchar '.') variables .>>. term |>> Abstraction

    /// Accept a lambda term application or a single operand.
    let application = operand .>>. applicationOpt |>> Application

    termRef.Value <- choice [application; abstraction]

    /// Accept a variable declaration.
    let declaration = pstring "let" >>. !<variable 

    /// Accept a variable declaration with assignment.
    let definition = !>declaration .>> pchar '=' .>>. !<term |>> Definition

    /// Accept an expression or an empty string.
    let expressionOpt = choice [definition; term |>> Result; preturn Epsilon]

    /// Accept an expression or an empty string followed by end of the input.
    let expression = expressionOpt .>> inputEnd
