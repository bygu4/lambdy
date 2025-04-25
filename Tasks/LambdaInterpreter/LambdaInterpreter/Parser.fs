namespace LambdaInterpreter

open FParsec
open Primary

/// Module dealing with the source text parsing.
module Parser =

    /// Accept 0 or more spaces.
    let whitespaceOpt = spaces

    /// Accept 1 of more spaces.
    let whitespace = spaces1

    /// Accept optional whitespace on the left of the given `parser`.
    let ( ?< ) parser = whitespaceOpt >>. parser

    /// Accept optional whitespace on the right of the given `parser`.
    let ( ?> ) parser = parser .>> whitespaceOpt

    /// Accept whitespace on the left of the given `parser`.
    let ( !< ) parser = whitespace >>. parser

    /// Accept whitespace on the right of the given `parser`.
    let ( !> ) parser = parser .>> whitespace

    /// Accept the end of line with optional whitespace.
    let lineEnd = (?<)(skipNewline <|> eof)

    /// Accept the variable name.
    let variable: Parser<Variable, unit> =
        let isFirstVariableChar c = isLetter c
        let isVariableChar c = isLetter c || isDigit c || c = '_'

        many1Satisfy2 isFirstVariableChar isVariableChar |>> Name

    /// Accept one or more of variable names.
    let variables = variable .>>. many !<variable |>> fun (h, t) -> h :: t

    /// Accept a primary lambda term representation.
    let term, termRef = createParserForwardedToRef ()

    /// Accept an operand in lambda term application.
    let operand =
        between ((?>)(pchar '(')) ((?<)(pchar ')')) term |>> Brackets
        <|> (variable |>> Variable)

    /// Accept a lambda abstraction.
    let abstraction = between (pchar '\\') (pchar '.') variables .>>. term |>> Abstraction

    /// Accept an optional lambda term application.
    let applicationOpt, applicationOptRef = createParserForwardedToRef ()

    applicationOptRef.Value <-
        !<operand .>>. applicationOpt |>> Apply
        <|> preturn ApplicationOpt.Epsilon

    /// Accept a lambda term application or a single operand.
    let application = operand .>>. applicationOpt |>> Application

    termRef.Value <- choice [application; abstraction]

    /// Accept a variable declaration.
    let declaration = pstring "let" >>. !<variable 

    /// Accept a variable declaration with initialization.
    let definition = !>declaration .>> pchar '=' .>>. !<term |>> Definition

    /// Accept an optional expression of a program.
    let expressionOpt = choice [definition; term |>> Result; preturn Epsilon]

    /// Accept an expression of a program or a line break.
    let expression = expressionOpt .>> lineEnd

    /// Accept a program as a list of expressions.
    let program = many expression
