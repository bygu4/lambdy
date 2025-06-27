namespace Lambdy.Syntax

open Lambdy.Syntax.ParseTree

/// Module containing finalized lambda term definitions with utility.
module LambdaTerm =

    /// The finalized lambda term representation.
    type LambdaTerm =
        | Variable of Variable
        | Abstraction of Variable * LambdaTerm
        | Application of LambdaTerm * LambdaTerm

    /// Get a string representation of the given lambda `term`.
    /// Use `parenthesized` to surround term with brackets if it's not a variable.
    /// Use `closing` to indicate that term is last in the application sequence.
    /// Use CPS with the given `cont`.
    [<TailCall>]
    let rec private toStringHelper term parenthesized closing cont =
        match term with
        | Variable (Name var) -> cont var
        | Application (left, right) ->
            let closedAbstraction = parenthesized || closing
            let parenthesizedLeft = left.IsAbstraction

            let parenthesizedRight =
                right.IsApplication || right.IsAbstraction && not closedAbstraction

            toStringHelper
                left
                parenthesizedLeft
                false
                (fun leftStr ->
                    toStringHelper
                        right
                        parenthesizedRight
                        closing
                        (fun rightStr ->
                            let termStr = $"{leftStr} {rightStr}"

                            if parenthesized then $"({termStr})" else termStr
                            |> cont
                        )
                )
        | Abstraction (Name var, term) ->
            toStringHelper
                term
                false
                true
                (fun termStr ->
                    let termStr = $"\\{var}.{termStr}"

                    if parenthesized then $"({termStr})" else termStr
                    |> cont
                )

    /// Get a string representation of the given lambda `term`.
    let toString term = toStringHelper term false true id

    /// Get a string representation of the given lambda `term` surrounded with brackets if it's not a variable.
    let toStringParenthesized term = toStringHelper term true true id
