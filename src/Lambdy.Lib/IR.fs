namespace Lambdy.Lib

open Lambdy.Lib.Continuation
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

    let churchNumeral n =
        let rec churchNumeral' n cont =
            match n with
            | 0us -> irVar (Name "x") |> cont
            | n -> churchNumeral' (n - 1us) (fun rest -> irApp (irVar (Name "f")) rest |> cont) in

        churchNumeral' n (fun app -> irAbs (Name "f") (irAbs (Name "x") app))

    // ============= IR to string conversion ==============================

    let irToString strict parenthesize ir =
        let rec irToString' strict parenthesize closing ir =
            continuation {
                match ir with
                | Variable (Name var, _) -> return var
                | Numeral num -> return num.ToString ()
                | Abstraction (Name var, _, body) ->
                    let! bodyStr = irToString' strict strict true body
                    let termStr = $"{lambdaChars.[0]}{var}.{bodyStr}"
                    return if parenthesize then $"({termStr})" else termStr
                | Application (left, right) ->
                    let closedAbstraction = parenthesize || closing
                    let parenthesizeLeft = strict || left.IsAbstraction

                    let parenthesizeRight =
                        strict || right.IsApplication || right.IsAbstraction && not closedAbstraction

                    let! leftStr = irToString' strict parenthesizeLeft false left
                    let! rightStr = irToString' strict parenthesizeRight closing right
                    let termStr = $"{leftStr} {rightStr}"
                    return if parenthesize then $"({termStr})" else termStr
            } in

        irToString' strict parenthesize true ir id

    // ============= Obtaining free variables =============================

    let freeVars expand term =
        let rec freeVars' expand term =
            continuation {
                match term with
                | Variable (var, Some boundValue) when expand ->
                    let! valueVars = freeVars' expand boundValue
                    return Set.add var valueVars
                | Variable (var, _) -> return set [ var ]
                | Numeral _ -> return Set.empty
                | Abstraction (var, _, body) ->
                    let! bodyVars = freeVars' expand body
                    return Set.remove var bodyVars
                | Application (left, right) ->
                    let! leftVars = freeVars' expand left
                    let! rightVars = freeVars' expand right
                    return Set.union leftVars rightVars
            } in

        freeVars' expand term id

    let rec nextFreeVar (Name prefix as var) freeVars =
        if not (Set.contains var freeVars) then
            var
        else
            nextFreeVar (Name <| prefix + "'") freeVars
