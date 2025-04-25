namespace LambdaInterpreter

open System
open Primary

/// Module dealing with finalized syntax trees.
module AST =

    /// The definition of lambda term.
    type LambdaTerm =
        | Variable of Variable
        | Abstraction of Variable * LambdaTerm
        | Application of LambdaTerm * LambdaTerm

    /// The finalized expression representation.
    type Expression =
        | Definition of Variable * LambdaTerm
        | Result of LambdaTerm

    /// The finalized program representation.
    type Program = Expression seq

    /// Build AST of a lambda term using the `primary` representation.
    let buildAST_Term: Primary.Term -> LambdaTerm = Unchecked.defaultof<_>
    let buildAST_Term_Ref = ref buildAST_Term

    /// Convert `primary` operand representation to the lambda term.
    let buildAST_Operand (primary: Primary.Operand) =
        match primary with
        | Primary.Variable var -> Variable var
        | Brackets term -> buildAST_Term term

    /// Build AST of lambda term application using term on the `left` and the rest on `right`.
    let rec buildAST_Application (left: LambdaTerm, right: Primary.ApplicationOpt) =
        match right with
        | Apply (operand, rest) ->
            let right = buildAST_Operand operand
            let partial = Application (left, right)
            buildAST_Application (partial, rest)
        | ApplicationOpt.Epsilon -> left

    buildAST_Term_Ref.Value <- fun primary ->
        match primary with
        | Primary.Application (operand, rest) ->
            let operand = buildAST_Operand operand
            buildAST_Application (operand, rest)
        | Primary.Abstraction (head :: tail, term) ->
            if tail.IsEmpty then Abstraction (head, buildAST_Term term)
            else Abstraction (head, buildAST_Term (Primary.Abstraction (tail, term)))
        | Primary.Abstraction ([], _) ->
            raise (ArgumentException "Abstraction received empty variable list")

    /// Build a finalized program representation from the `primary` one.
    let buildAST_Program (primary: Primary.Program) =
        seq {
            for expression in primary do
                match expression with
                | Primary.Definition (variable, term) ->
                    yield Definition (variable, buildAST_Term term)
                | Primary.Result term ->
                    yield Result (buildAST_Term term)
                | Epsilon -> ()
        }

    /// Get a string representation of the given lambda `term`.
    let rec toString (term: LambdaTerm) =
        match term with
        | Variable (Name var) -> var
        | Application (left, right) -> $"{toString left} {toString right}"
        | Abstraction (Name var, term) -> $"(\\{var}.{toString term})"
