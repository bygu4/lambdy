namespace Lambdy.Lib

open System

open Lambdy.Lib.Continuation
open Lambdy.Lib.ParseTree
open Lambdy.Lib.IR

/// Module dealing with finalized syntax trees of lambda expressions.
module AST =

    /// The finalized expression representation.
    type Expression =
        | Binding of variable : VariableName * value : IR
        | Result of value : IR
        | Command of command : Command
        | Empty

    /// Build AST of a lambda term using the `primary` representation.
    let buildTermAST primary =
        let rec buildTermAST' primary =

            /// Convert `primary` operand representation to the lambda term.
            let inline buildOperandAST primary =
                continuation {
                    match primary with
                    | ParseTree.Variable var -> return irVar var
                    | ParseTree.Numeral num -> return irNum num
                    | Brackets term -> return! buildTermAST' term
                }

            /// Build AST of lambda term application using term on the `left` and the rest on `right`.
            let rec buildApplicationAST left right =
                continuation {
                    match right with
                    | WithContinuation (operand, rest) ->
                        let! right = buildOperandAST operand
                        let partial = irApp left right
                        return! buildApplicationAST partial rest
                    | ClosingAbstraction abs ->
                        let! right = buildTermAST' abs
                        return irApp left right
                    | Epsilon -> return left
                }

            continuation {
                match primary with
                | ParseTree.Application (operand, rest) ->
                    let! operandAST = buildOperandAST operand
                    return! buildApplicationAST operandAST rest
                | ParseTree.Abstraction (Names (head :: []), body) ->
                    let! bodyAST = buildTermAST' body
                    return irAbs head bodyAST
                | ParseTree.Abstraction (Names (head :: tail), body) ->
                    let rest = ParseTree.Abstraction (Names tail, body)
                    let! restAST = buildTermAST' rest
                    return irAbs head restAST
                | ParseTree.Abstraction (Names [], _) ->
                    return raise <| ArgumentException "Abstraction received empty variable list."
            } in

        buildTermAST' primary id

    /// Build a finalized expression representation from the `primary` one.
    let buildExpressionAST primary =
        match primary with
        | ParseTree.Binding (variable, term) -> Binding (variable, buildTermAST term)
        | ParseTree.Result term -> Result <| buildTermAST term
        | ParseTree.Command command -> Command command
        | ParseTree.Empty -> Empty
