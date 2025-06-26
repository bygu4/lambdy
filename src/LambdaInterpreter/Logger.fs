namespace LambdaInterpreter

open LambdaInterpreter.Syntax
open AST

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

/// Class for managing execution logs.
/// Use `verbose` option to print logs to the console.
type Logger(?verbose : bool) =
    let verbose = defaultArg verbose false

    /// Print a log message using the given `record` according to verbosity.
    member _.Log (record : LogRecord) =
        if verbose then
            match record with
            | StartedReducing term -> $"{toString term}\n|"
            | DoneReducing -> "V"
            | Reducing term -> $"|  reducing {toString term} ..."
            | AlphaConversion (Name x, Name y) -> $"|  [alpha-conversion]: {x} -> {y}"
            | BetaReduction (source, term, Name var, sub) ->
                $"|  [beta-reduction]: {toString source} -> {toStringParenthesized term}[{var} := {toString sub}]"
            | Substitution (Name var, sub) -> $"|  [substitution]: {var} -> {toString sub}"
            | UnableToReduce term -> $"|  unable to reduce {toString term}\n|  term didn't change after beta-reduction"
            | NewDefinition (Name var, term) -> $"(!) definition: {var} = {toString term}"
            | DefinitionsReset -> "(!) definitions were reset"
            | DisplayingDefinitions -> "Defined variables in order:"
            | NoVariablesDefined -> "-  no variables were defined yet"
            |> printfn "%s"
