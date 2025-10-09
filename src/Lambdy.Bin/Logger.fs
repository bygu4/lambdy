namespace Lambdy.Bin

open System

open Lambdy.Lib.ParseTree
open Lambdy.Lib.IR
open Lambdy.Lib.LambdaTerm
open Lambdy.Lib.Interfaces.ILambdaInterpreter
open Lambdy.Lib.Interfaces.ILogger
open Lambdy.Bin.Verbosity
open Lambdy.Bin.ColorScheme
open Lambdy.Bin.Help

module Logger =

    type Logger(colorScheme : ColorScheme, ?verbosity : Verbosity, ?lineNumber : bool, ?strictTerms : bool) =
        let verbosity = defaultArg verbosity Verbosity.Minimal
        let lineNumber = defaultArg lineNumber false
        let strictTerms = defaultArg strictTerms false
        let lockObject = new obj ()

        // ------------- Helpers -------------------------------

        let printLog prefixColor msgColor prefix msg =
            if prefix <> String.Empty then
                Logger.PrintfColorized prefixColor $"[{prefix}]: "

            Logger.PrintfnColorized msgColor msg

        let printNormalLog = printLog colorScheme.AccentColor colorScheme.TextColor

        let printDetailedLog = printLog colorScheme.NoteColor colorScheme.TextColor

        let printDiagnosticLog =
            printLog colorScheme.DiagnosticColor colorScheme.DiagnosticColor

        // ------------- Minimal -------------------------------

        let handleWaitingForInput () = printf InputPointer

        let handleSyntaxHelp () = printSyntaxHelp ()

        let handleCurrentScope bindings =
            let mutable i = 1

            for Name var, boundValue in bindings do
                printfn $"{Indent}{i}) {var} <- {irToString strictTerms false boundValue}"
                i <- i + 1

        let handleInterpreterOutput (Output (Line line, result)) =
            let lineStr = if lineNumber then $"[Line {line}]: " else String.Empty

            match result with
            | Ok result -> Logger.PrintSuccess (lineStr + lamToString strictTerms false result)
            | Error message -> Logger.PrintError (lineStr + message)

        // ------------- Normal --------------------------------

        let handleStartedReducing = irToString strictTerms false >> printfn "%s"

        let handleCurrentTerm = irToString strictTerms false >> printfn "%s"

        let handleUnbinding (Name var) term =
            printNormalLog "subst" $"{var} -> {irToString strictTerms false term}"

        let handleAlphaConversion term (Name var) (Name sub) =
            printNormalLog "alpha" $"{irToString strictTerms true term}: {var} -> {sub}"

        let handleBetaReduction src body (Name var) sub =
            let srcStr = irToString strictTerms false src
            let bodyStr = irToString strictTerms true body
            let subStr = irToString strictTerms false sub
            printNormalLog "beta" $"{srcStr} -> {bodyStr}[{var} := {subStr}]"

        // ------------- Detailed ------------------------------

        let handleCurrentlyReducing term =
            printfn $"reducing {irToString strictTerms false term} ..."

        let handleNewBinding (Name var) value =
            printDetailedLog "bind" $"{var} <- {irToString strictTerms false value}"

        let handleWasReset () =
            printDetailedLog "reset" "cleared current scope"

        // ------------- Diagnostic ----------------------------

        let handleSubstitution (Name var) sub =
            printDiagnosticLog "subst" $"{var} -> {irToString strictTerms false sub}"

        let handleBoundValue (Name var) value =
            printDiagnosticLog "bind" $"{var} <- {irToString strictTerms false value}"

        let handleDelayedConversion term (Name var) (Name sub) =
            printDiagnosticLog "delay" $"{irToString strictTerms false term}: {var} -> {sub}"

        // ------------- Arbitrary record handling -------------

        let handleRecord =
            function
            | Minimal WaitingForInput -> handleWaitingForInput ()
            | Minimal SyntaxHelp -> handleSyntaxHelp ()
            | Minimal (CurrentScope bindings) -> handleCurrentScope bindings
            | Minimal (InterpreterOutput output) -> handleInterpreterOutput output
            | Normal (StartedReducing term) -> handleStartedReducing term
            | Normal (CurrentTerm term) -> handleCurrentTerm term
            | Normal (Unbinding (var, term)) -> handleUnbinding var term
            | Normal (AlphaConversion (term, var, sub)) -> handleAlphaConversion term var sub
            | Normal (BetaReduction (src, body, var, sub)) -> handleBetaReduction src body var sub
            | Detailed (CurrentlyReducing term) -> handleCurrentlyReducing term
            | Detailed (NewBinding (var, value)) -> handleNewBinding var value
            | Detailed WasReset -> handleWasReset ()
            | Diagnostic (Substitution (var, sub)) -> handleSubstitution var sub
            | Diagnostic (BoundValue (var, value)) -> handleBoundValue var value
            | Diagnostic (DelayedConversion (term, var, sub)) -> handleDelayedConversion term var sub

        // ------------- Members -------------------------------

        static member PrintSuccess str =
            Logger.PrintfnColorized defaultSuccessColor str

        static member PrintError str =
            Logger.PrintfnColorized defaultErrorColor str

        static member private PrintfColorized color str =
            Console.ForegroundColor <- color
            printf "%s" str
            Console.ResetColor ()

        static member private PrintfnColorized color str =
            Logger.PrintfColorized color (str + "\n")

        member _.LogAsync record =
            task {
                lock
                    lockObject
                    (fun () ->
                        if matchesTargetVerbosity record verbosity then
                            handleRecord record
                    )
            }

        member _.ClearBuffer () = Console.Clear ()

        interface ILogger with
            member this.LogAsync record = this.LogAsync record
            member this.ClearBuffer () = this.ClearBuffer ()
