namespace Lambdy.Syntax

open System

open ParseTree
open LambdaTerm

/// Module dealing with finalized syntax trees of lambda expressions.
module AST =

    /// The finalized expression representation.
    type Expression =
        | Definition of Variable * LambdaTerm
        | Result of LambdaTerm
        | Command of SpecialCommand
        | Empty

    /// Build AST of a lambda term using the `primary` representation.
    /// Use CPS with the given `cont`.
    [<TailCall>]
    let rec private buildTermASTHelper primary cont =

        /// Convert `primary` operand representation to the lambda term.
        let buildOperandAST primary cont =
            match primary with
            | ParseTree.Variable var -> Variable var |> cont
            | Brackets term -> buildTermASTHelper term cont

        /// Build AST of lambda term application using term on the `left` and the rest on `right`.
        let rec buildApplicationAST left right cont =
            match right with
            | WithContinuation (operand, rest) ->
                buildOperandAST
                    operand
                    (fun right ->
                        let partial = Application (left, right)
                        buildApplicationAST partial rest cont
                    )
            | ClosingAbstraction abs -> buildTermASTHelper abs (fun right -> Application (left, right) |> cont)
            | Epsilon -> cont left

        match primary with
        | ParseTree.Application (operand, rest) ->
            buildOperandAST operand (fun operandAST -> buildApplicationAST operandAST rest cont)
        | ParseTree.Abstraction (head :: tail, term) ->
            if tail.IsEmpty then
                buildTermASTHelper term (fun termAST -> Abstraction (head, termAST) |> cont)
            else
                buildTermASTHelper
                    (ParseTree.Abstraction (tail, term))
                    (fun termAST -> Abstraction (head, termAST) |> cont)
        | ParseTree.Abstraction ([], _) -> raise (ArgumentException "Abstraction received empty variable list")

    /// Build AST of a lambda term using the `primary` representation.
    let buildTermAST primary = buildTermASTHelper primary id

    /// Build a finalized expression representation from the `primary` one.
    let buildExpressionAST primary =
        match primary with
        | ParseTree.Definition (variable, term) -> Definition (variable, buildTermAST term)
        | ParseTree.Result term -> Result (buildTermAST term)
        | ParseTree.Command command -> Command command
        | ParseTree.Empty -> Empty
