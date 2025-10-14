namespace Lambdy.Lib

open FParsec

open Lambdy.Lib.Literals
open Lambdy.Lib.ParseTree

/// Module dealing with the source text parsing.
module Parsers =

    /// Accept 0 or more whitespaces.
    let whitespaceOpt = spaces

    /// Accept 1 or more whitespaces.
    let whitespace = spaces1

    /// Accept optional whitespace on the right of the given `parser`.
    let (?>) parser = parser .>> whitespaceOpt

    /// Accept whitespace on the right of the given `parser`.
    let (!>) parser = parser .>> whitespace

    /// Accept a comment as a special char followed by any string.
    let comment = pchar CommentChar >>. skipManyTill anyChar eof

    /// Accept optional comment followed by the end of input.
    let inputEnd = optional comment .>> eof

    /// Accept the variable name.
    let variable =

        /// Modifies the given `reply` to check that its result is not a keyword.
        let exceptReserved (reply : Reply<string>) =
            if reply.Status = ReplyStatus.Ok && isReserved reply.Result then
                Reply (
                    ReplyStatus.Error,
                    ErrorMessage.Unexpected $"'{reply.Result}' is reserved as a keyword"
                    |> ErrorMessageList
                )
            else
                reply

        regex VariablePattern >> exceptReserved |>> Name

    /// Accept one or more of variable names, separated and optionally surrounded by whitespace.
    let variables = sepEndBy1 variable whitespace |>> Names

    /// Accept a primary lambda term representation.
    let term, termRef = createParserForwardedToRef ()

    /// Accept an operand in lambda term application.
    let operand =

        /// Accept a parenthesized lambda term.
        let termPar = between ((?>) (pchar '(')) (pchar ')') ((?>) term)

        choice [ variable |>> Variable ; termPar |>> Brackets ]

    /// Accept a lambda abstraction.
    let abstraction =

        /// Accept a single lambda symbol.
        let lambdaSymbol = choice [ pchar MainLambdaChar ; pchar SecondaryLambdaChar ]

        /// Accept a character separating variables and body of the abstraction.
        let separator = pchar SeparatorChar

        between ((?>) lambdaSymbol) ((?>) separator) variables .>>. term |>> Abstraction

    /// Accept a lambda term application or a single operand.
    let application =

        /// Accept an optional lambda term application.
        let applicationOpt, applicationOptRef = createParserForwardedToRef ()

        applicationOptRef.Value <-

            /// Accept an operand application with continuation.
            let applicationOpt' = operand .>>. applicationOpt |>> WithContinuation

            /// Accept a final abstraction in the application sequence.
            let applicationOpt'' = abstraction |>> ClosingAbstraction

            choice
                [
                    whitespace >>? choice [ applicationOpt' ; applicationOpt'' ]
                    preturn Epsilon
                ]

        operand .>>. applicationOpt |>> Application

    termRef.Value <- choice [ application ; abstraction ]

    /// Accept a variable declaration.
    let declaration = (!>) (pstring DeclarationKeyword) >>. variable

    /// Accept a variable declaration with assignment.
    let binding = !>declaration .>> (!>) (pchar '=') .>>. term |>> Binding

    /// Accept a special interpreter command.
    let command =

        /// Accept a keyword for resetting defined variables.
        let reset = pstring ResetKeyword >>. preturn (Command Reset)

        /// Accept a keyword for displaying defined variables.
        let scope = pstring ScopeKeyword >>. preturn (Command Scope)

        /// Accept a keyword for displaying help.
        let help = pstring HelpKeyword >>. preturn (Command Help)

        /// Accept a keyword for clearing the console buffer.
        let clear = pstring ClearKeyword >>. preturn (Command Clear)

        /// Accept a keyword for exiting the interpreter.
        let exit = pstring ExitKeyword >>. preturn (Command Exit)

        pchar CommandChar >>. choice [ reset ; scope ; help ; clear ; exit ]

    /// Accept an expression or an empty string followed by end of the input.
    let expression interactive : Parser<Expression, unit> =

        /// Accept an expression or an empty string.
        let expressionOpt =

            let options =
                if interactive then
                    [ attempt term |>> Result ; binding ; command ; preturn Empty ]
                else
                    [ attempt term |>> Result ; binding ; preturn Empty ]

            choice options

        (?>) expressionOpt .>> inputEnd
