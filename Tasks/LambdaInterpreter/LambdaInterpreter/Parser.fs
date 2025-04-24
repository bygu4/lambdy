namespace LambdaInterpreter

open FParsec

module Parser =

    let whitespaceOpt = spaces

    let whitespace = spaces1

    let ( ?< ) parser = whitespaceOpt >>. parser

    let ( ?> ) parser = parser .>> whitespaceOpt

    let ( !< ) parser = whitespace >>. parser

    let ( !> ) parser = parser .>> whitespace

    let lineEnd = (?<)(skipNewline <|> eof)

    let variable: Parser<Variable, obj> =
        let isFirstVariableChar c = isLetter c
        let isVariableChar c = isLetter c || isDigit c || c = '_'

        many1Satisfy2 isFirstVariableChar isVariableChar |>> Name

    let variables = variable .>>. many !<variable |>> fun (h, t) -> h :: t

    let term, termRef = createParserForwardedToRef ()

    let operand =
        between ((?>)(pchar '(')) ((?<)(pchar ')')) term |>> Brackets
        <|> (variable |>> Variable)

    let abstraction = between (pchar '\\') (pchar '.') variables .>>. term |>> Abstraction

    let applicationOpt, applicationOptRef = createParserForwardedToRef ()

    applicationOptRef.Value <-
        !<operand .>>. applicationOpt |>> Apply
        <|> preturn ApplicationOpt.Epsilon

    let application = operand .>>. applicationOpt |>> Application

    termRef.Value <- choice [application; abstraction]

    let declaration = pstring "let" >>. !<variable 

    let definition = !>declaration .>> pchar '=' .>>. !<term |>> Definition

    let expressionOpt = choice [definition; term |>> Result; preturn Epsilon]

    let expression = expressionOpt .>> lineEnd

    let program = many expression
