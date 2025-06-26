namespace Lambdy.Syntax

/// Module defining primary syntax constructions.
module ParseTree =

    /// A variable of specific name.
    type Variable = | Name of string

    /// A list of variables.
    type Variables = Variable list

    /// A special command for managing the interpreter execution.
    type SpecialCommand =
        | Reset
        | Display
        | Help
        | Clear
        | Exit

    /// A primary term representation.
    type Term =
        | Application of Operand * OptionalApplication
        | Abstraction of Variables * Term

    /// Optional lambda term application.
    and OptionalApplication =
        | WithContinuation of Operand * OptionalApplication
        | ClosingAbstraction of Term
        | Epsilon

    /// An operand in lambda term application.
    and Operand =
        | Variable of Variable
        | Brackets of Term

    /// An expression to be interpreted.
    type Expression =
        | Definition of Variable * Term
        | Result of Term
        | Command of SpecialCommand
        | Empty
