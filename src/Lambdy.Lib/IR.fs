namespace Lambdy.Lib

open Lambdy.Lib.ParseTree

module IR =

    type IR =
        | Variable of VariableName * BoundValue
        | Abstraction of VariableName * Conversion * IR
        | Application of IR * IR

    and BoundValue = IR option
    and Conversion = VariableName option

    let variable name = Variable (name, None)
    let abstraction var body = Abstraction (var, None, body)
    let application left right = Application (left, right)

    [<TailCall>]
    let rec private toString' parenthesize closing ir cont =
        match ir with
        | Variable (Name var, _) -> cont var
        | Application (left, right) ->
            let closedAbstraction = parenthesize || closing
            let parenthesizedLeft = left.IsAbstraction

            let parenthesizedRight =
                right.IsApplication || right.IsAbstraction && not closedAbstraction

            toString'
                parenthesizedLeft
                false
                left
                (fun leftStr ->
                    toString'
                        parenthesizedRight
                        closing
                        right
                        (fun rightStr ->
                            let termStr = $"{leftStr} {rightStr}"

                            if parenthesize then $"({termStr})" else termStr
                            |> cont
                        )
                )
        | Abstraction (Name var, _, term) ->
            toString'
                false
                true
                term
                (fun termStr ->
                    let termStr = $"\\{var}.{termStr}"

                    if parenthesize then $"({termStr})" else termStr
                    |> cont
                )

    let toString parenthesize ir = toString' parenthesize true ir id
