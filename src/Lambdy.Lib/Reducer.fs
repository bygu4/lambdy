namespace Lambdy.Lib

open Lambdy.Lib.ParseTree
open Lambdy.Lib.LambdaTerm
open Lambdy.Lib.IR
open Lambdy.Lib.Interfaces.ILogger
open Lambdy.Lib.Interfaces.IReducer

module Reducer =

    type Reducer(?maxReductions : int, ?logger : ILogger) =
        let mutable bindings : (VariableName * IR) list = []

        // ------------- Diagnostic logging --------------------

        let substitute var sub =
            ILogger.TryLog (logger, Diagnostic <| Substitution (var, sub))
            sub

        let bindValue var bindings =
            match Map.tryFind var bindings with
            | Some value ->
                ILogger.TryLog (logger, Diagnostic <| BoundValue (var, value))
                irVarBound var (Some value)
            | None -> irVar var

        let delayConversion term x conv body =
            match conv with
            | Some y ->
                ILogger.TryLog (logger, Diagnostic <| DelayedConversion (term, x, y))
                irAbsConv x conv body
            | None -> irAbsConv x conv body

        // ------------- Value binding -------------------------

        [<TailCall>]
        let rec bindMany' bindings term cont =
            match term with

            | _ when Map.isEmpty bindings -> cont term

            | Variable (x, _) -> bindValue x bindings |> cont

            | Abstraction (x, _, body) ->
                let bindings = Map.remove x bindings

                if Map.isEmpty bindings then
                    cont term
                else

                let vars = set <| Map.keys bindings
                let freeVarsT = freeVars false body

                let freeVarsS =
                    Set.intersect vars freeVarsT
                    |> Seq.map (fun var -> bindings.[var] |> freeVars true)
                    |> Set.unionMany

                let conv =
                    if Set.contains x freeVarsS then
                        Some <| nextFreeVar x (Set.union freeVarsT freeVarsS)
                    else
                        None

                bindMany' bindings body (fun boundBody -> delayConversion term x conv boundBody |> cont)

            | Application (left, right) ->
                bindMany'
                    bindings
                    left
                    (fun boundLeft -> bindMany' bindings right (fun boundRight -> irApp boundLeft boundRight |> cont))

        let bindMany bindings term = bindMany' bindings term id

        // ------------- Substitution --------------------------

        [<TailCall>]
        let rec substitute' var sub term cont =
            match term with

            | Variable (x, _) when x = var -> substitute var sub |> cont

            | Abstraction (x, conv, body) when x <> var ->
                let freeVarsT = freeVars false body
                let freeVarsS = freeVars true sub

                let willSubstitute = Set.contains var freeVarsT
                let capturedX = Set.contains x freeVarsS

                let capturedConv =
                    match conv with
                    | Some y -> Set.contains y freeVarsS
                    | None -> false

                if willSubstitute then
                    let freeVarsT = freeVars true body
                    let y = nextFreeVar x (Set.union freeVarsT freeVarsS)

                    if capturedX then
                        ILogger.TryLog (logger, Normal <| AlphaConversion (term, x, y))

                        substitute'
                            x
                            (irVar y)
                            body
                            (fun convBody -> substitute' var sub convBody (fun subsBody -> irAbs y subsBody |> cont))
                    else
                        let conv = if capturedConv then Some y else conv
                        substitute' var sub body (fun subsBody -> delayConversion term x conv subsBody |> cont)

                else
                    cont term

            | Application (left, right) ->
                substitute'
                    var
                    sub
                    left
                    (fun subsLeft -> substitute' var sub right (fun subsRight -> irApp subsLeft subsRight |> cont))

            | _ -> cont term

        let substitute var sub term = substitute' var sub term id

        // ------------- Normal logging ------------------------

        let currentTerm term root =
            ILogger.TryLog (logger, Normal <| CurrentTerm (root term))
            term

        let unbind var boundValue root =
            ILogger.TryLog (logger, Normal <| Unbinding (var, boundValue))
            let res = boundValue
            currentTerm res root

        let alphaConversion term x y body root =
            ILogger.TryLog (logger, Normal <| AlphaConversion (term, x, y))
            let res = irAbs y (substitute x (irVar y) body)
            currentTerm res root

        let betaReduction term body var sub root =
            ILogger.TryLog (logger, Normal <| BetaReduction (term, body, var, sub))
            let res = substitute var sub body
            currentTerm res root

        // ------------- Reduction -----------------------------

        [<TailCall>]
        let rec reduce' ret count term root cont =

            let inline maybe step =
                function
                | Ok result -> step result
                | _ as err -> err

            let inline maybePassTo cont step = maybe step >> cont

            if maxReductions.IsSome && count > maxReductions.Value then
                cont <| Error "Max recursion depth exceeded during the reduction."
            else

            match term with

            | Variable (var, Some boundValue) -> reduce' ret count (unbind var boundValue root) root cont

            | Abstraction (x, Some y, body) when not ret ->
                reduce' ret count (alphaConversion term x y body root) root cont

            | Abstraction (var, None, body) when not ret ->
                reduce'
                    ret
                    count
                    body
                    (fun body -> irAbs var body |> root)
                    (maybePassTo cont (fun reducedBody -> irAbs var reducedBody |> Ok))

            | Application (Abstraction (var, _, body), sub) ->
                reduce' ret (count + 1) (betaReduction term body var sub root) root cont

            | Application (left, right) ->
                reduce'
                    true
                    count
                    left
                    (fun left -> irApp left right |> root)
                    (maybe (fun reducedLeft ->
                        match reducedLeft with
                        | Abstraction _ -> reduce' ret count (irApp reducedLeft right) root cont
                        | _ ->
                            reduce'
                                false
                                count
                                right
                                (fun right -> irApp reducedLeft right |> root)
                                (maybePassTo cont (fun reducedRight -> irApp reducedLeft reducedRight |> Ok))
                    ))

            | _ -> cont <| Ok term

        let reduce term = reduce' false 0 term id id

        // ------------- IR to closure -------------------------

        [<TailCall>]
        let rec closure' term root cont =
            match term with
            | Variable (var, None) -> lamVar var |> cont
            | Variable (var, Some boundValue) -> closure' (unbind var boundValue root) root cont
            | Abstraction (var, None, body) ->
                closure' body (fun body -> irAbs var body |> root) (fun bodyClosure -> lamAbs var bodyClosure |> cont)
            | Abstraction (x, Some y, body) -> closure' (alphaConversion term x y body root) root cont
            | Application (left, right) ->
                closure'
                    left
                    (fun left -> irApp left right |> root)
                    (fun leftClosure ->
                        closure'
                            right
                            (fun right -> irApp (toIR leftClosure) right |> root)
                            (fun rightClosure -> lamApp leftClosure rightClosure |> cont)
                    )

        let closure term = closure' term id id

        // ------------- Members -------------------------------

        member _.Bindings = List.rev bindings

        member _.AddBinding (var, value) =
            let boundValue = bindMany (bindings |> List.rev |> Map.ofList) value
            bindings <- (var, boundValue) :: bindings

        member _.ResetBindings () = bindings <- []

        member _.Reduce term =
            term
            |> bindMany (bindings |> List.rev |> Map.ofList)
            |> reduce
            |> function
                | Ok result -> closure result |> Ok
                | Error msg -> Error msg

        interface IReducer with
            member this.Bindings = this.Bindings
            member this.AddBinding (var, term) = this.AddBinding (var, term)
            member this.ResetBindings () = this.ResetBindings ()
            member this.Reduce term = this.Reduce term
