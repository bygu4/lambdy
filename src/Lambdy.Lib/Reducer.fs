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

        let delayConversion term x alpha body =
            match alpha with
            | Some y ->
                ILogger.TryLog (logger, Diagnostic <| DelayedAlpha (term, x, y))
                irAbsAlpha x y body
            | None -> irAbs x body

        // ============= Value binding ========================================

        [<TailCall>]
        let rec bindMany' bindings term cont =
            match term with

            | _ when Map.isEmpty bindings -> cont term

            | Variable (x, _) -> bindValue term x bindings |> cont

            | Numeral _ -> cont term

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
                        Some <| nextFreeVar x (Set.union freeVarsBody freeVarsSub)
                    else
                        None

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
                    | Some y -> Set.contains y freeVarsSub
                    | None -> false

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
                        let alpha = if capturedAlpha then Some y else alpha
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

        let expandVar var boundValue root =
            let res = boundValue
            ILogger.TryLog (logger, Normal <| Expansion (var, res))
            currentTerm res root

        let expandNum num root =
            let name = num.ToString () |> Name
            let res = churchNumeral num
            ILogger.TryLog (logger, Normal <| Expansion (name, res))
            currentTerm res root

        let alphaConversion src x y body root =
            ILogger.TryLog (logger, Normal <| AlphaConversion (src, x, y))
            let res = irAbs y (substitute x (irVar y) body)
            currentTerm res root

        let etaReduction src left root =
            ILogger.TryLog (logger, Normal <| EtaReduction (src, left))
            let res = left
            currentTerm res root

        let betaReduction src body var sub root =
            ILogger.TryLog (logger, Normal <| BetaReduction (src, body, var, sub))
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
                | Some limit when reductionCount >= limit -> Error $"Exceeded the reduction limit: {limit}." |> cont
                | _ ->
                    reductionCount <- reductionCount + 1
                    betaReduction term body var sub root |> Ok |> cont

            let inline reduceVar curStrategy term var boundValue root cont =
                match boundValue with
                | Some boundValue -> reduce' curStrategy (expandVar var boundValue root) root cont
                | _ -> Ok term |> cont

            let inline reduceNum num root cont =
                reduce' curStrategy (expandNum num root) root cont

            let inline reduceAbs (Strategy (_, Strong strong) as curStrategy) term var alpha body root cont =
                match alpha, body with

                | _, Application (left, right) when
                    etaConversions
                    && right = irVar var
                    && not <| Set.contains var (freeVars false left)
                    ->
                    reduce' curStrategy (etaReduction term left root) root cont

                | Some alpha, body when strong ->
                    reduce' curStrategy (alphaConversion term var alpha body root) root cont

                | _, body when strong ->
                    reduce'
                        curStrategy
                        body
                        (fun body -> irAbs var body |> root)
                        (maybe cont (fun reducedBody -> irAbs var reducedBody |> Ok |> cont))

                | _ -> Ok term |> cont

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
            | Numeral num -> reduceNum num root cont
            | Abstraction (var, alpha, body) -> reduceAbs curStrategy term var alpha body root cont
            | Application (left, right) -> reduceApp curStrategy false term left right root cont

        let reduce term = reduce' strategy term id id

        // ============= IR to closure ========================================

        [<TailCall>]
        let rec closure' term root cont =
            match term with
            | Variable (var, None) -> lamVar var |> cont
            | Variable (var, Some boundValue) -> closure' (expandVar var boundValue root) root cont
            | Numeral num -> closure' (expandNum num root) root cont
            | Abstraction (var, _, Application (left, right)) when
                etaConversions
                && right = irVar var
                && not <| Set.contains var (freeVars false left)
                ->
                closure' (etaReduction term left root) root cont
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

        // ============= Members ==============================================

        member _.Scope = List.rev bindings

        member this.AddBinding (var, value) =
            let boundValue = bindMany (this.Scope |> Map.ofList) value
            bindings <- (var, boundValue) :: bindings

        member _.Reset () = bindings <- []

        member this.Reduce term =
            reductionCount <- 0

            term
            |> bindMany (this.Scope |> Map.ofList)
            |> reduce
            |> function
                | Ok reducedTerm -> Success (closure reducedTerm, reductionCount)
                | Error msg -> Failure msg

        interface IReducer with
            member this.Scope = this.Scope
            member this.AddBinding (var, term) = this.AddBinding (var, term)
            member this.Reset () = this.Reset ()
            member this.Reduce term = this.Reduce term
