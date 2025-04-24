namespace LambdaInterpreter

/// Module dealing with finalized syntax trees.
module AST =

    /// The definition of lambda term.
    type LambdaTerm =
        | Variable of Variable
        | Abstraction of Variable * LambdaTerm
        | Application of LambdaTerm * LambdaTerm

    /// The finalized variable definition.
    type Definition = Variable * LambdaTerm
