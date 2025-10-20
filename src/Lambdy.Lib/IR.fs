namespace Lambdy.Lib

open Lambdy.Lib.Literals
open Lambdy.Lib.ParseTree

module IR =

    type IR =
        | Variable of name : VariableName * boundValue : IR option
        | Numeral of number : uint16
        | Abstraction of variable : VariableName * delayedAlpha : VariableName option * body : IR
        | Application of left : IR * right : IR

    // ============= Constructors =========================================

    let inline irVar name = Variable (name, None)
    let inline irVarBound name boundValue = Variable (name, Some boundValue)
    let inline irNum num = Numeral num
    let inline irAbs var body = Abstraction (var, None, body)
    let inline irAbsAlpha var alpha body = Abstraction (var, Some alpha, body)
    let inline irApp left right = Application (left, right)

    // ============= Church numerals ======================================

    [<TailCall>]
    let rec private churchNumeral' n cont =
        match n with
        | 0us -> irVar (Name "x") |> cont
        | n -> churchNumeral' (n - 1us) (fun numeral -> irApp (irVar (Name "f")) numeral |> cont)

    let churchNumeral n =
        churchNumeral' n (fun app -> irAbs (Name "f") (irAbs (Name "x") app))

    // ============= IR to string conversion ==============================

    [<TailCall>]
    let rec private irToString' strict parenthesize closing ir cont =
        match ir with
        | Variable (Name var, _) -> cont var
        | Numeral num -> num.ToString () |> cont
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
                    let termStr = $"{lambdaChars.[0]}{var}.{bodyStr}"

                    if parenthesize then $"({termStr})" else termStr
                    |> cont
                )

    let irToString strict parenthesize ir =
        irToString' strict parenthesize true ir id

    // ============= Obtaining free variables =============================

    [<TailCall>]
    let rec private freeVars' closure term cont =
        match term with
        | Variable (var, Some boundValue) when closure ->
            freeVars' closure boundValue (fun valueVars -> Set.add var valueVars |> cont)
        | Variable (var, _) -> set [ var ] |> cont
        | Numeral _ -> Set.empty |> cont
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
