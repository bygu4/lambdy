namespace LambdaInterpreter.Syntax

open System
open Primary

/// Module dealing with finalized syntax trees of lambda expressions.
module AST =

    /// The finalized lambda term representation.
    type LambdaTerm =
        | Variable of Variable
        | Abstraction of Variable * LambdaTerm
        | Application of LambdaTerm * LambdaTerm

    /// The finalized expression representation.
    type Expression =
        | Definition of Variable * LambdaTerm
        | Result of LambdaTerm
        | Command of SpecialCommand
        | Empty

    /// Build AST of a lambda term using the `primary` representation.
    /// Use CPS with the given `cont`.
    [<TailCall>]
    let rec private buildTermAST_CPS (primary: Primary.Term) cont =

        /// Convert `primary` operand representation to the lambda term.
        let buildOperandAST (primary: Primary.Operand) cont =
            match primary with
            | Primary.Variable var -> Variable var |> cont
            | Brackets term -> buildTermAST_CPS term cont

        /// Build AST of lambda term application using term on the `left` and the rest on `right`.
        let rec buildApplicationAST (left: LambdaTerm) (right: Primary.ApplicationOpt) cont =
            match right with
            | WithContinuation (operand, rest) ->
                buildOperandAST operand (fun right ->
                    let partial = Application (left, right)
                    buildApplicationAST partial rest cont
                )
            | FinalAbstraction abs ->
                buildTermAST_CPS abs (fun right ->
                    Application (left, right) |> cont
                )
            | Epsilon -> cont left

        match primary with
        | Primary.Application (operand, rest) ->
            buildOperandAST operand (fun operandAST ->
                buildApplicationAST operandAST rest cont
            )
        | Primary.Abstraction (head :: tail, term) ->
            if tail.IsEmpty then
                buildTermAST_CPS term (fun termAST ->
                    Abstraction (head, termAST) |> cont
                )
            else
                buildTermAST_CPS (Primary.Abstraction (tail, term)) (fun termAST ->
                    Abstraction (head, termAST) |> cont
                )
        | Primary.Abstraction ([], _) ->
            raise (ArgumentException "Abstraction received empty variable list")

    /// Build AST of a lambda term using the `primary` representation.
    let buildTermAST (primary: Primary.Term) = buildTermAST_CPS primary id

    /// Build a finalized expression representation from the `primary` one.
    let buildExpressionAST (primary: Primary.Expression) =
        match primary with
        | Primary.Definition (variable, term) -> Definition (variable, buildTermAST term)
        | Primary.Result term -> Result (buildTermAST term)
        | Primary.Command command -> Command command
        | Primary.Empty -> Empty

    /// Get a string representation of the given lambda `term`.
    /// Use `parenthesized` to surround term with brackets if it's not a variable.
    /// Use `closing` to indicate that term is last in the application sequence.
    /// Use CPS with the given `cont`.
    [<TailCall>]
    let rec private toStringCPS term parenthesized closing cont = 
        match term with
        | Variable (Name var) -> cont var
        | Application (left, right) ->
            let parenthesizedLeft = left.IsAbstraction
            let parenthesizedRight = right.IsApplication || right.IsAbstraction && not closing
            toStringCPS left parenthesizedLeft false (fun leftStr ->
                toStringCPS right parenthesizedRight closing (fun rightStr ->
                    let termStr = $"{leftStr} {rightStr}"
                    if parenthesized then $"({termStr})" else termStr
                    |> cont
                )
            )
        | Abstraction (Name var, term) ->
            toStringCPS term false true (fun termStr ->
                let termStr = $"\\{var}.{termStr}"
                if parenthesized then $"({termStr})" else termStr
                |> cont
            )

    /// Get a string representation of the given lambda `term`.
    let toString (term: LambdaTerm) =
        toStringCPS term false true id

    /// Get a string representation of the given lambda `term` surrounded with brackets if it's not a variable.
    let toStringParenthesized (term: LambdaTerm) =
        toStringCPS term true true id
