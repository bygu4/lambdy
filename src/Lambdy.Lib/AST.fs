namespace Lambdy.Lib

open System

open Lambdy.Lib.ParseTree
open Lambdy.Lib.IR

/// Module dealing with finalized syntax trees of lambda expressions.
module AST =

    /// The finalized expression representation.
    type Expression =
        | Binding of VariableName * IR
        | Result of IR
        | Command of SpecialCommand
        | Empty

    /// Build AST of a lambda term using the `primary` representation.
    /// Use CPS with the given `cont`.
    [<TailCall>]
    let rec buildTermAST' primary cont =

        /// Convert `primary` operand representation to the lambda term.
        let inline buildOperandAST primary cont =
            match primary with
            | ParseTree.Variable var -> irVar var |> cont
            | Brackets term -> buildTermAST' term cont

        /// Build AST of lambda term application using term on the `left` and the rest on `right`.
        let rec buildApplicationAST left right cont =
            match right with
            | WithContinuation (operand, rest) ->
                buildOperandAST
                    operand
                    (fun right ->
                        let partial = irApp left right
                        buildApplicationAST partial rest cont
                    )
            | ClosingAbstraction abs -> buildTermAST' abs (fun right -> irApp left right |> cont)
            | Epsilon -> cont left

        match primary with
        | ParseTree.Application (operand, rest) ->
            buildOperandAST operand (fun operandAST -> buildApplicationAST operandAST rest cont)
        | ParseTree.Abstraction (head :: tail, body) ->
            if tail.IsEmpty then
                buildTermAST' body (fun bodyAST -> irAbs head bodyAST |> cont)
            else
                buildTermAST' (ParseTree.Abstraction (tail, body)) (fun absAST -> irAbs head absAST |> cont)
        | ParseTree.Abstraction ([], _) -> raise (ArgumentException "Abstraction received empty variable list")

    /// Build AST of a lambda term using the `primary` representation.
    let buildTermAST primary = buildTermAST' primary id

    /// Build a finalized expression representation from the `primary` one.
    let buildExpressionAST primary =
        match primary with
        | ParseTree.Binding (variable, term) -> Binding (variable, buildTermAST term)
        | ParseTree.Result term -> Result (buildTermAST term)
        | ParseTree.Command command -> Command command
        | ParseTree.Empty -> Empty
