namespace LambdaInterpreter

/// A variable of specific name.
type Variable = Name of string

/// A list of variables.
type Variables = Variable list

/// A primary term representation.
type Term =
    | Application of Operand * ApplicationOpt
    | Abstraction of Variables * Term

/// Optional lambda term application.
and ApplicationOpt =
    | Apply of Operand * ApplicationOpt
    | Epsilon

/// An operand in lambda term application.
and Operand =
    | Variable of Variable
    | Brackets of Term

/// An expression as a logical unit of the program.
type Expression =
    | Definition of Variable * Term
    | Result of Term
    | Epsilon

/// Program as a list of expressions.
type Program = Expression list
