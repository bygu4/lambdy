namespace Lambdy.Interfaces

open Lambdy.Syntax.ParseTree
open Lambdy.Syntax.LambdaTerm

/// An interface representing a class handling the lambda term reduction logic.
[<Interface>]
type IReducer =

    // Get of defined variables.
    abstract member Bindings : (VariableName * LambdaTerm) list

    // Perform beta-reduction of the given lambda term according to defined variables.
    abstract member Reduce : LambdaTerm -> InterpretationResult

    // Define a variable to be substituted with the given term.
    abstract member AddBinding : VariableName * LambdaTerm -> unit

    // Clear the list of defined variables.
    abstract member ResetBindings : unit -> unit
