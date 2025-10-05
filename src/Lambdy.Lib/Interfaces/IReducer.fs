namespace Lambdy.Lib.Interfaces

open Lambdy.Lib.ParseTree
open Lambdy.Lib.IR
open Lambdy.Lib.LambdaTerm

module IReducer =

    type ReductionResult = Result<LambdaTerm, string>

    /// An interface representing a class handling the lambda term reduction logic.
    [<Interface>]
    type IReducer =

        // Get list of defined variables.
        abstract member Bindings : (VariableName * IR) list

        // Perform beta-reduction of the given lambda term according to defined variables.
        abstract member Reduce : IR -> ReductionResult

        // Define a variable to be substituted with the given term.
        abstract member AddBinding : VariableName * IR -> unit

        // Clear the list of defined variables.
        abstract member ResetBindings : unit -> unit
