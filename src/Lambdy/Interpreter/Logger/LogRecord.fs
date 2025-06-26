namespace Lambdy.Interpreter.Logger

open Lambdy.Syntax.ParseTree
open Lambdy.Syntax.LambdaTerm

/// Log record of reducer execution.
type LogRecord =
    | StartedReducing of LambdaTerm
    | DoneReducing
    | Reducing of LambdaTerm
    | AlphaConversion of Variable * Variable
    | BetaReduction of LambdaTerm * LambdaTerm * Variable * LambdaTerm
    | Substitution of Variable * LambdaTerm
    | UnableToReduce of LambdaTerm
    | NewDefinition of Variable * LambdaTerm
    | DefinitionsReset
    | DisplayingDefinitions
    | NoVariablesDefined
