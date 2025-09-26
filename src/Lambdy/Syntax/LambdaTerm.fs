namespace Lambdy.Syntax

open Lambdy.Syntax.ParseTree

// Module containing finalized lambda term definitions.
module LambdaTerm =

    /// The finalized lambda term representation.
    type LambdaTerm =
        | Variable of VariableName
        | Abstraction of VariableName * LambdaTerm
        | Application of LambdaTerm * LambdaTerm

        member this.ToString parenthesize = this.ToString' parenthesize true id

        override this.ToString () = this.ToString false

        /// Get a string representation of the given lambda `term`.
        /// Use `parenthesize` to surround term with brackets if it's not a variable.
        /// Use `closing` to indicate that term is last in the application sequence.
        /// Use CPS with the given `cont`.
        [<TailCall>]
        member private this.ToString' parenthesize closing cont =
            match this with
            | Variable (Name var) -> cont var
            | Application (left, right) ->
                let closedAbstraction = parenthesize || closing
                let parenthesizedLeft = left.IsAbstraction

                let parenthesizedRight =
                    right.IsApplication || right.IsAbstraction && not closedAbstraction

                left.ToString'
                    parenthesizedLeft
                    false
                    (fun leftStr ->
                        right.ToString'
                            parenthesizedRight
                            closing
                            (fun rightStr ->
                                let termStr = $"{leftStr} {rightStr}"

                                if parenthesize then $"({termStr})" else termStr
                                |> cont
                            )
                    )
            | Abstraction (Name var, term) ->
                term.ToString'
                    false
                    true
                    (fun termStr ->
                        let termStr = $"\\{var}.{termStr}"

                        if parenthesize then $"({termStr})" else termStr
                        |> cont
                    )

    let toString parenthesize (term : LambdaTerm) = term.ToString parenthesize
