namespace Lambdy.Lib

open Lambdy.Lib.Literals
open Lambdy.Lib.ParseTree

module IR =

    type IR =
        | Variable of VariableName * BoundValue
        | Abstraction of VariableName * DelayedAlpha * IR
        | Application of IR * IR

    and BoundValue = | BoundValue of IR option
    and DelayedAlpha = | DelayedAlpha of VariableName option

    // ============= Constructors =========================================

    let irBoundVal = Some >> BoundValue
    let irDelayedAlpha = Some >> DelayedAlpha

    let inline irVar name = Variable (name, BoundValue None)
    let inline irVarBound name boundValue = Variable (name, irBoundVal boundValue)

    let inline irAbs var body =
        Abstraction (var, DelayedAlpha None, body)

    let inline irAbsAlpha var alpha body =
        Abstraction (var, irDelayedAlpha alpha, body)

    let inline irApp left right = Application (left, right)

    // ============= IR to string conversion ==============================

    [<TailCall>]
    let rec private irToString' strict parenthesize closing ir cont =
        match ir with
        | Variable (Name var, _) -> cont var
        | Application (left, right) ->
            let closedAbstraction = parenthesize || closing
            let parenthesizeLeft = strict || left.IsAbstraction

            let parenthesizeRight =
                strict || right.IsApplication || right.IsAbstraction && not closedAbstraction

            irToString'
                strict
                parenthesizeLeft
                false
                left
                (fun leftStr ->
                    irToString'
                        strict
                        parenthesizeRight
                        closing
                        right
                        (fun rightStr ->
                            let termStr = $"{leftStr} {rightStr}"

                            if parenthesize then $"({termStr})" else termStr
                            |> cont
                        )
                )
        | Abstraction (Name var, _, body) ->
            irToString'
                strict
                strict
                true
                body
                (fun bodyStr ->
                    let termStr = $"{MainLambdaChar}{var}.{bodyStr}"

                    if parenthesize then $"({termStr})" else termStr
                    |> cont
                )

    let irToString strict parenthesize ir =
        irToString' strict parenthesize true ir id

    // ============= Obtaining free variables =============================

    [<TailCall>]
    let rec private freeVars' closure term cont =
        match term with
        | Variable (var, BoundValue (Some boundValue)) when closure ->
            freeVars' closure boundValue (fun valueVars -> Set.add var valueVars |> cont)
        | Variable (var, _) -> set [ var ] |> cont
        | Abstraction (var, _, body) -> freeVars' closure body (fun bodyVars -> Set.remove var bodyVars |> cont)
        | Application (left, right) ->
            freeVars'
                closure
                left
                (fun leftVars -> freeVars' closure right (fun rightVars -> Set.union leftVars rightVars |> cont))

    let freeVars closure term = freeVars' closure term id

    [<TailCall>]
    let rec nextFreeVar (Name prefix as var) freeVars =
        if not (Set.contains var freeVars) then
            var
        else
            nextFreeVar (Name <| prefix + "'") freeVars
