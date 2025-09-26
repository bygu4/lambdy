namespace Lambdy.Lib

/// Module defining primary syntax constructions.
module ParseTree =

    /// A variable of specific name.
    type VariableName = | Name of string

    /// A list of variables.
    type VariableNames = VariableName list

    /// A special command for managing the interpreter execution.
    type SpecialCommand =
        | Reset
        | Scope
        | Help
        | Clear
        | Exit

    /// A primary term representation.
    type Term =
        | Application of Operand * OptionalApplication
        | Abstraction of VariableNames * Term

    /// Optional lambda term application.
    and OptionalApplication =
        | WithContinuation of Operand * OptionalApplication
        | ClosingAbstraction of Term
        | Epsilon

    /// An operand in lambda term application.
    and Operand =
        | Variable of VariableName
        | Brackets of Term

    /// An expression to be interpreted.
    type Expression =
        | Definition of VariableName * Term
        | Result of Term
        | Command of SpecialCommand
        | Empty
