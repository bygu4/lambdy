namespace Lambdy.Interpreter

open Lambdy.Syntax.ParseTree
open Lambdy.Syntax.LambdaTerm

module IR =

    type IR =
        | Variable of VariableName * BoundValue
        | Abstraction of VariableName * Conversion * IR
        | Application of IR * IR

    and BoundValue = IR option
    and Conversion = VariableName option

    [<TailCall>]
    let rec private toIR' term cont =
        match term with
        | LambdaTerm.Variable var -> Variable (var, None) |> cont
        | LambdaTerm.Abstraction (var, body) -> toIR' body (fun bodyIR -> Abstraction (var, None, bodyIR) |> cont)
        | LambdaTerm.Application (left, right) ->
            toIR' left (fun leftIR -> toIR' right (fun rightIR -> Application (leftIR, rightIR) |> cont))

    let toIR term = toIR' term id

    [<TailCall>]
    let rec private toLambdaTerm' ir cont =
        match ir with
        | Variable (var, _) -> LambdaTerm.Variable var |> cont
        | Abstraction (var, _, body) ->
            toLambdaTerm' body (fun bodyTerm -> LambdaTerm.Abstraction (var, bodyTerm) |> cont)
        | Application (left, right) ->
            toLambdaTerm'
                left
                (fun leftTerm ->
                    toLambdaTerm' right (fun rightTerm -> LambdaTerm.Application (leftTerm, rightTerm) |> cont)
                )

    let toLambdaTerm ir = toLambdaTerm' ir id
