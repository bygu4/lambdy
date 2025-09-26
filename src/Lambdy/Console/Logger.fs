namespace Lambdy.Console

open System

open Lambdy.Syntax.ParseTree
open Lambdy.Syntax.LambdaTerm
open Lambdy.Interfaces
open Help

/// Class for managing interpreter execution logs.
/// Use `verbose` option to print logs to the console.
type Logger(?verbose : bool, ?lineNumber : bool) =
    let verbose = defaultArg verbose false
    let lineNumber = defaultArg lineNumber false

    let printColorized color str =
        Console.ForegroundColor <- color
        printfn "%s" str
        Console.ResetColor ()

    let handleWaitingForInput () = printf InputPointer

    let handleSyntaxHelp () = printSyntaxHelp ()

    let handleClear () = Console.Clear ()

    let handleListBindings bindings =
        let mutable i = 1

        for Name var, boundValue in bindings do
            printfn $"{Indent}{i}) {var} := {boundValue}"
            i <- i + 1

    let handleOutput (InterpreterOutput.Output (Line line, result)) =
        if lineNumber then
            printColorized ConsoleColor.Gray $"[Line {line}]: "

        match result with
        | Ok result -> printColorized ConsoleColor.Green result
        | Error message -> printColorized ConsoleColor.Red message

    let handleStartedReducing = ignore

    let handleDoneReducing = ignore

    let handleCurrentTerm = _.ToString() >> printfn "%s"

    let handleSubstitution (Name var) sub = printfn $"[subst]: {var} -> {sub}"

    let handleAlphaConversion term (Name var) (Name sub) =
        printfn $"[alpha] ({term}): {var} -> {sub}"

    let handleBetaReduction src (body : LambdaTerm) (Name var) sub =
        printfn $"[beta]: {src} -> {body.ToString true}[{var} := {sub}]"

    let handleNewBinding (Name var) value = printfn $"[bind]: {var} -> {value}"

    let handleResetBindings () =
        printfn "[reset]: cleared bindings list"

    let handleRecord =
        function
        | Interactive WaitingForInput -> handleWaitingForInput ()
        | Interactive SyntaxHelp -> handleSyntaxHelp ()
        | Interactive Clear -> handleClear ()
        | Interactive (ListBindings bindings) -> handleListBindings bindings
        | Interactive (Output output) -> handleOutput output
        | Reduction (StartedReducing term) -> handleStartedReducing term
        | Reduction (DoneReducing term) -> handleDoneReducing term
        | Reduction (CurrentTerm term) -> handleCurrentTerm term
        | Transformation (Substitution (var, sub)) -> handleSubstitution var sub
        | Transformation (AlphaConversion (term, var, sub)) -> handleAlphaConversion term var sub
        | Transformation (BetaReduction (src, body, var, sub)) -> handleBetaReduction src body var sub
        | Binding (NewBinding (var, value)) -> handleNewBinding var value
        | Binding ResetBindings -> handleResetBindings ()

    member _.Log = handleRecord

    interface ILogger with
        member this.Log record = this.Log record
