namespace Lambdy.Lib

open Lambdy.Lib.ParseTree

module IR =

    type IR =
        | Variable of VariableName * BoundValue
        | Abstraction of VariableName * DelayedConversion * IR
        | Application of IR * IR

    and BoundValue = IR option
    and DelayedConversion = VariableName option

    let inline irVar name = Variable (name, None)
    let inline irVarBound name boundValue = Variable (name, boundValue)
    let inline irAbs var body = Abstraction (var, None, body)
    let inline irAbsConv var conv body = Abstraction (var, conv, body)
    let inline irApp left right = Application (left, right)

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
                    let termStr = $"\\{var}.{bodyStr}"

                    if parenthesize then $"({termStr})" else termStr
                    |> cont
                )

    let irToString strict parenthesize ir =
        irToString' strict parenthesize true ir id

    [<TailCall>]
    let rec private freeVars' closure term cont =
        match term with
        | Variable (var, Some boundValue) when closure ->
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
