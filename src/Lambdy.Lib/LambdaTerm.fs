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

    let inline lamVar name = Variable name
    let inline lamAbs var body = Abstraction (var, body)
    let inline lamApp left right = Application (left, right)

    [<TailCall>]
    let rec private toIR' term cont =
        match term with
        | Variable var -> irVar var |> cont
        | Abstraction (var, body) -> toIR' body (fun bodyIR -> irAbs var bodyIR |> cont)
        | Application (left, right) ->
            toIR' left (fun leftIR -> toIR' right (fun rightIR -> irApp leftIR rightIR |> cont))

    let toIR term = toIR' term id

    [<TailCall>]
    let rec private toLambdaTerm' ir cont =
        match ir with
        | IR.Variable (var, _) -> lamVar var |> cont
        | IR.Abstraction (var, _, body) -> toLambdaTerm' body (fun bodyTerm -> lamAbs var bodyTerm |> cont)
        | IR.Application (left, right) ->
            toLambdaTerm'
                left
                (fun leftTerm -> toLambdaTerm' right (fun rightTerm -> lamApp leftTerm rightTerm |> cont))

    let toLambdaTerm ir = toLambdaTerm' ir id

    let lamToString strict parenthesize = toIR >> irToString strict parenthesize
