namespace Lambdy.Lib

open Lambdy.Lib.ParseTree
open Lambdy.Lib.IR

// Module containing finalized lambda term definitions.
module LambdaTerm =

    /// The finalized lambda term representation.
    type LambdaTerm =
        | Variable of VariableName
        | Abstraction of VariableName * LambdaTerm
        | Application of LambdaTerm * LambdaTerm

    [<TailCall>]
    let rec private toIR' term cont =
        match term with
        | Variable var -> IR.Variable (var, None) |> cont
        | Abstraction (var, body) -> toIR' body (fun bodyIR -> IR.Abstraction (var, None, bodyIR) |> cont)
        | Application (left, right) ->
            toIR' left (fun leftIR -> toIR' right (fun rightIR -> IR.Application (leftIR, rightIR) |> cont))

    let toIR term = toIR' term id

    [<TailCall>]
    let rec private toLambdaTerm' ir cont =
        match ir with
        | IR.Variable (var, _) -> Variable var |> cont
        | IR.Abstraction (var, _, body) -> toLambdaTerm' body (fun bodyTerm -> Abstraction (var, bodyTerm) |> cont)
        | IR.Application (left, right) ->
            toLambdaTerm'
                left
                (fun leftTerm -> toLambdaTerm' right (fun rightTerm -> Application (leftTerm, rightTerm) |> cont))

    let toLambdaTerm ir = toLambdaTerm' ir id

    let toString parenthesize term = term |> toIR |> toString parenthesize
