namespace Lambdy.Syntax

open FParsec

open Literals
open ParseTree

/// Module dealing with the source text parsing.
module Parsers =

    /// Accept 0 or more whitespaces.
    let whitespaceOpt = spaces

    /// Accept 1 or more whitespaces.
    let whitespace = spaces1

    /// Accept optional whitespace on the left of the given `parser`.
    let (?<) parser = whitespaceOpt >>. parser

    /// Accept optional whitespace on the right of the given `parser`.
    let (?>) parser = parser .>> whitespaceOpt

    /// Accept whitespace on the left of the given `parser`.
    let (!<) parser = whitespace >>. parser

    /// Accept whitespace on the right of the given `parser`.
    let (!>) parser = parser .>> whitespace

    /// Accept optional whitespace followed by end of the input.
    let inputEnd = (?<) eof

    /// Modifies the given `reply` to check that its result is not a keyword.
    let exceptKeyword (reply : Reply<string>) =
        if reply.Status = ReplyStatus.Ok && isKeyword reply.Result then
            Reply (
                ReplyStatus.Error,
                ErrorMessage.Unexpected $"'{reply.Result}' is reserved as a keyword"
                |> ErrorMessageList
            )
        else
            reply

    /// Accept the variable name.
    let variable = regex VariablePattern >> exceptKeyword |>> Name

    /// Accept one or more of variable names, separated and optionally surrounded by whitespace.
    let variables = (?<) (sepEndBy1 variable whitespace)

    /// Accept a primary lambda term representation.
    let term, termRef = createParserForwardedToRef ()

    /// Accept a parenthesized lambda term.
    let termPar = between ((?>) (pchar '(')) ((?<) (pchar ')')) term

    /// Accept an operand in lambda term application.
    let operand = choice [ variable |>> Variable ; termPar |>> Brackets ]

    /// Accept a lambda abstraction.
    let abstraction =
        between (pchar '\\') (pchar '.') variables .>>. (?<) term |>> Abstraction

    /// Accept an optional lambda term application.
    let applicationOpt, applicationOptRef = createParserForwardedToRef ()

    /// Accept an operand application with continuation.
    let applicationOpt' = attempt (!<operand .>>. applicationOpt) |>> WithContinuation

    /// Accept a final abstraction in the application sequence.
    let applicationOpt'' = attempt !<abstraction |>> ClosingAbstraction

    applicationOptRef.Value <- choice [ applicationOpt' ; applicationOpt'' ; preturn Epsilon ]

    /// Accept a lambda term application or a single operand.
    let application = operand .>>. applicationOpt |>> Application

    termRef.Value <- choice [ application ; abstraction ]

    /// Accept a variable declaration.
    let declaration = pstring DeclarationKeyword >>. !<variable

    /// Accept a variable declaration with assignment.
    let definition = !>declaration .>> pchar '=' .>>. !<term |>> Definition

    /// Accept a keyword for resetting defined variables.
    let reset = pstring ResetKeyword >>. preturn (Command Reset)

    /// Accept a keyword for displaying defined variables.
    let display = pstring DisplayKeyword >>. preturn (Command Display)

    /// Accept a keyword for displaying help.
    let help = pstring HelpKeyword >>. preturn (Command Help)

    /// Accept a keyword for clearing the console buffer.
    let clear = pstring ClearKeyword >>. preturn (Command Clear)

    /// Accept a keyword for exiting the interpreter.
    let exit = pstring ExitKeyword >>. preturn (Command Exit)

    /// Accept a special interpreter command.
    let command = choice [ reset ; display ; help ; clear ; exit ]

    /// Accept an expression or an empty string.
    let expressionOpt : Parser<Expression, unit> =
        choice [ attempt term |>> Result ; definition ; command ; preturn Empty ]

    /// Accept an expression or an empty string followed by end of the input.
    let expression = expressionOpt .>> inputEnd
