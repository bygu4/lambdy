namespace Lambdy.Bin

open System

open Lambdy.Lib.ParseTree
open Lambdy.Lib.LambdaTerm
open Lambdy.Lib.Interfaces
open Lambdy.Bin.Help

module Logger =

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
            | Minimal WaitingForInput -> handleWaitingForInput ()
            | Minimal SyntaxHelp -> handleSyntaxHelp ()
            | Minimal ClearBuffer -> handleClear ()
            | Minimal (CurrentScope bindings) -> handleListBindings bindings
            | Minimal (Output output) -> handleOutput output
            | Normal (StartedReducing term) -> handleStartedReducing term
            | Normal (DoneReducing term) -> handleDoneReducing term
            | Normal (CurrentTerm term) -> handleCurrentTerm term
            | Detailed (Substitution (var, sub)) -> handleSubstitution var sub
            | Detailed (AlphaConversion (term, var, sub)) -> handleAlphaConversion term var sub
            | Detailed (BetaReduction (src, body, var, sub)) -> handleBetaReduction src body var sub
            | Diagnostic (NewBinding (var, value)) -> handleNewBinding var value
            | Diagnostic WasReset -> handleResetBindings ()

        member _.Log = handleRecord

        interface ILogger with
            member this.Log record = this.Log record
