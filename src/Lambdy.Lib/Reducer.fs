namespace Lambdy.Lib

open Lambdy.Lib.ParseTree
open Lambdy.Lib.LambdaTerm
open Lambdy.Lib.IR
open Lambdy.Lib.Strategy
open Lambdy.Lib.Interfaces.ILogger
open Lambdy.Lib.Interfaces.IReducer

module Reducer =

    type Reducer(?strategy : Strategy, ?etaConversions : bool, ?reductionLimit : int, ?logger : ILogger) =
        let strategy = defaultArg strategy normalOrder
        let etaConversions = defaultArg etaConversions true

        let mutable bindings : (VariableName * IR) list = []
        let mutable reductionCount = 0

        // ============= Diagnostic logging ===================================

        let substitute var sub =
            ILogger.TryLog (logger, Diagnostic <| Substitution (var, sub))
            sub

        let bindValue term var bindings =
            match Map.tryFind var bindings with
            | Some value ->
                ILogger.TryLog (logger, Diagnostic <| ValueBinding (var, value))
                irVarBound var value
            | None -> term

        let delayConversion term x (DelayedAlpha alpha) body =
            match alpha with
            | Some y ->
                ILogger.TryLog (logger, Diagnostic <| DelayedAlphaConv (term, x, y))
                irAbsAlpha x y body
            | None -> irAbs x body

        // ============= Value binding ========================================

        [<TailCall>]
        let rec bindMany' bindings term cont =
            match term with

            | _ when Map.isEmpty bindings -> cont term

            | Variable (x, _) -> bindValue term x bindings |> cont

            | Abstraction (x, _, body) ->
                let bindings = Map.remove x bindings

                if Map.isEmpty bindings then
                    cont term
                else

                let vars = set <| Map.keys bindings
                let freeVarsBody = freeVars false body

                let freeVarsSub =
                    Set.intersect vars freeVarsBody
                    |> Seq.map (fun var -> bindings.[var] |> freeVars true)
                    |> Set.unionMany

                let alpha =
                    if Set.contains x freeVarsSub then
                        irDelayedAlpha <| nextFreeVar x (Set.union freeVarsBody freeVarsSub)
                    else
                        DelayedAlpha None

                bindMany' bindings body (fun boundBody -> delayConversion term x alpha boundBody |> cont)

            | Application (left, right) ->
                bindMany'
                    bindings
                    left
                    (fun boundLeft -> bindMany' bindings right (fun boundRight -> irApp boundLeft boundRight |> cont))

        let bindMany bindings term = bindMany' bindings term id

        // ============= Substitution =========================================

        [<TailCall>]
        let rec substitute' var sub term cont =
            match term with

            | Variable (x, _) when x = var -> substitute var sub |> cont

            | Abstraction (x, alpha, body) when x <> var ->
                let freeVarsBody = freeVars false body
                let freeVarsSub = freeVars true sub

                let willSubstitute = Set.contains var freeVarsBody
                let capturedX = Set.contains x freeVarsSub

                let capturedAlpha =
                    match alpha with
                    | DelayedAlpha (Some y) -> Set.contains y freeVarsSub
                    | DelayedAlpha None -> false

                if willSubstitute then
                    let freeVarsBody = freeVars true body
                    let y = nextFreeVar x (Set.union freeVarsBody freeVarsSub)

                    if capturedX then
                        ILogger.TryLog (logger, Normal <| AlphaConversion (term, x, y))

                        substitute'
                            x
                            (irVar y)
                            body
                            (fun convBody -> substitute' var sub convBody (fun subsBody -> irAbs y subsBody |> cont))
                    else
                        let alpha = if capturedAlpha then irDelayedAlpha y else alpha
                        substitute' var sub body (fun subsBody -> delayConversion term x alpha subsBody |> cont)

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

        // ============= Normal logging =======================================

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

        let etaConversion term left root =
            ILogger.TryLog (logger, Normal <| EtaConversion (term, left))
            let res = left
            currentTerm res root

        let betaReduction term body var sub root =
            ILogger.TryLog (logger, Normal <| BetaReduction (term, body, var, sub))
            let res = substitute var sub body
            currentTerm res root

        // ============= Reduction ============================================

        [<TailCall>]
        let rec reduce' curStrategy term root cont =

            let inline maybe fail step =
                function
                | Ok result -> step result
                | _ as err -> fail err

            let inline tryBetaReduction term body var sub root cont =
                match reductionLimit with
                | Some limit when reductionCount >= limit -> cont <| Error $"Exceeded the reduction limit: {limit}."
                | _ ->
                    reductionCount <- reductionCount + 1
                    betaReduction term body var sub root |> Ok |> cont

            let inline reduceVar curStrategy term var boundValue root cont =
                match boundValue with
                | BoundValue (Some boundValue) -> reduce' curStrategy (unbind var boundValue root) root cont
                | _ -> cont <| Ok term

            let inline reduceAbs (Strategy (_, Strong strong) as curStrategy) term var alpha body root cont =
                match alpha, body with

                | _, Application (left, right) when
                    etaConversions
                    && right = irVar var
                    && not <| Set.contains var (freeVars false left)
                    ->
                    reduce' curStrategy (etaConversion term left root) root cont

                | DelayedAlpha (Some alpha), body when strong ->
                    reduce' curStrategy (alphaConversion term var alpha body root) root cont

                | _, body when strong ->
                    reduce'
                        curStrategy
                        body
                        (fun body -> irAbs var body |> root)
                        (maybe cont (fun reducedBody -> irAbs var reducedBody |> Ok |> cont))

                | _ -> cont <| Ok term

            let rec reduceApp (Strategy (Strict strict, _) as curStrategy) finish term left right root cont =
                match left with

                | Abstraction (var, _, body) when finish || not strict ->
                    tryBetaReduction
                        term
                        body
                        var
                        right
                        root
                        (maybe cont (fun reducedTerm -> reduce' curStrategy reducedTerm root cont))

                | _ when finish && strict -> irApp left right |> Ok |> cont

                | _ when finish ->
                    reduce'
                        strategy
                        right
                        (fun right -> irApp left right |> root)
                        (maybe cont (fun reducedRight -> irApp left reducedRight |> Ok |> cont))

                | _ when strict ->
                    reduce'
                        curStrategy
                        left
                        (fun left -> irApp left right |> root)
                        (maybe
                            cont
                            (fun reducedLeft ->
                                reduce'
                                    curStrategy
                                    right
                                    (fun right -> irApp reducedLeft right |> root)
                                    (maybe
                                        cont
                                        (fun reducedRight ->
                                            reduceApp
                                                curStrategy
                                                true
                                                (irApp reducedLeft reducedRight)
                                                reducedLeft
                                                reducedRight
                                                root
                                                cont
                                        ))
                            ))

                | _ ->
                    reduce'
                        callByName
                        left
                        (fun left -> irApp left right |> root)
                        (maybe
                            cont
                            (fun reducedLeft ->
                                reduceApp curStrategy true (irApp reducedLeft right) reducedLeft right root cont
                            ))

            ILogger.TryLog (logger, Detailed <| CurrentlyReducing term)

            match term with
            | Variable (var, boundValue) -> reduceVar curStrategy term var boundValue root cont
            | Abstraction (var, alpha, body) -> reduceAbs curStrategy term var alpha body root cont
            | Application (left, right) -> reduceApp curStrategy false term left right root cont

        let reduce term = reduce' strategy term id id

        // ============= IR to closure ========================================

        [<TailCall>]
        let rec closure' term root cont =
            match term with
            | Variable (var, BoundValue None) -> lamVar var |> cont
            | Variable (var, BoundValue (Some boundValue)) -> closure' (unbind var boundValue root) root cont
            | Abstraction (var, _, Application (left, right)) when
                etaConversions
                && right = irVar var
                && not <| Set.contains var (freeVars false left)
                ->
                closure' (etaConversion term left root) root cont
            | Abstraction (var, DelayedAlpha None, body) ->
                closure' body (fun body -> irAbs var body |> root) (fun bodyClosure -> lamAbs var bodyClosure |> cont)
            | Abstraction (x, DelayedAlpha (Some y), body) -> closure' (alphaConversion term x y body root) root cont
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

        // ============= Members ==============================================

        member _.Scope = List.rev bindings

        member this.AddBinding (var, value) =
            let boundValue = bindMany (this.Scope |> Map.ofList) value
            bindings <- (var, boundValue) :: bindings

        member _.Reset () = bindings <- []

        member this.Reduce term =
            reductionCount <- 0

            let result = term |> bindMany (this.Scope |> Map.ofList) |> reduce

            match result, strategy with
            | Ok reducedTerm, Strategy (_, Strong true) ->
                Success (NormalForm <| toLambdaTerm reducedTerm, ReductionCount reductionCount)
            | Ok reducedTerm, _ -> Success (NormalForm <| closure reducedTerm, ReductionCount reductionCount)
            | Error msg, _ -> Failure <| ErrorMessage msg

        interface IReducer with
            member this.Scope = this.Scope
            member this.AddBinding (var, term) = this.AddBinding (var, term)
            member this.Reset () = this.Reset ()
            member this.Reduce term = this.Reduce term
