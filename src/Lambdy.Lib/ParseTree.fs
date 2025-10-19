namespace Lambdy.Lib

/// Module defining primary syntax constructions.
module ParseTree =

    /// A variable of specific name.
    type VariableName = | Name of string

    /// A list of variables.
    type VariableNames = | Names of VariableName list

    /// A special command for managing the interpreter execution.
    type Command =
        | Reset
        | Scope
        | Help
        | Clear
        | Exit

    /// A primary term representation.
    type Term =
        | Application of operand : Operand * rest : OptionalApplication
        | Abstraction of variables : VariableNames * body : Term

    /// Optional lambda term application.
    and OptionalApplication =
        | WithContinuation of operand : Operand * rest : OptionalApplication
        | ClosingAbstraction of abstraction : Term
        | Epsilon

    /// An operand in lambda term application.
    and Operand =
        | Variable of name : VariableName
        | Numeral of number : uint16
        | Brackets of term : Term

    /// An expression to be interpreted.
    type Expression =
        | Binding of variable : VariableName * value : Term
        | Result of value : Term
        | Command of command : Command
        | Empty
