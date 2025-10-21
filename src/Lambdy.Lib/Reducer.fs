namespace Lambdy.Lib

open Lambdy.Lib.Continuation
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

        let currentlyReducing term =
            ILogger.TryLog (logger, Diagnostic <| CurrentlyReducing term)
            term

        let substitute var sub =
            ILogger.TryLog (logger, Diagnostic <| Substitution (var, sub))
            sub

        let bindValue term var bindings =
            match Map.tryFind var bindings with
            | Some value ->
                ILogger.TryLog (logger, Diagnostic <| BoundValue (var, value))
                irVarBound var value
            | None -> term

        let delayConversion term x alpha body =
            match alpha with
            | Some y ->
                ILogger.TryLog (logger, Diagnostic <| DelayedAlpha (term, x, y))
                irAbsAlpha x y body
            | None -> irAbs x body

        // ============= Value binding ========================================

        let bindMany bindings term =
            let rec bindMany' bindings term =
                continuation {
                    match term with

                    | _ when Map.isEmpty bindings -> return term

                    | Variable (x, _) -> return bindValue term x bindings

                    | Numeral _ -> return term

                    | Abstraction (x, _, body) ->
                        let bindings = Map.remove x bindings

                        if Map.isEmpty bindings then
                            return term
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

                        let! boundBody = bindMany' bindings body
                        return delayConversion term x alpha boundBody

                    | Application (left, right) ->
                        let! boundLeft = bindMany' bindings left
                        let! boundRight = bindMany' bindings right
                        return irApp boundLeft boundRight
                } in

            bindMany' bindings term id

        // ============= Substitution =========================================

        let substitute var sub term =
            let rec substitute' var sub term =
                continuation {
                    match term with

                    | Variable (x, _) when x = var -> return substitute var sub

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
                                ILogger.TryLog (logger, Detailed <| AlphaConversion (term, x, y))

                                let! convBody = substitute' x (irVar y) body
                                let! subsBody = substitute' var sub convBody
                                return irAbs y subsBody
                            else
                                let alpha = if capturedAlpha then Some y else alpha
                                let! subsBody = substitute' var sub body
                                return delayConversion term x alpha subsBody
                        else
                            return term

                    | Application (left, right) ->
                        let! subsLeft = substitute' var sub left
                        let! subsRight = substitute' var sub right
                        return irApp subsLeft subsRight

                    | _ -> return term
                } in

            substitute' var sub term id

        // ============= Normal and detailed logging ==========================

        let currentTerm term root =
            ILogger.TryLog (logger, Normal <| CurrentTerm (reductionCount, root term))
            term

        let expandVar var boundValue root =
            let res = boundValue
            ILogger.TryLog (logger, Detailed <| Expansion (var, res))
            currentTerm res root

        let expandNum num root =
            let name = num.ToString () |> Name
            let res = churchNumeral num
            ILogger.TryLog (logger, Detailed <| Expansion (name, res))
            currentTerm res root

        let alphaConversion src x y body root =
            ILogger.TryLog (logger, Detailed <| AlphaConversion (src, x, y))
            let res = irAbs y (substitute x (irVar y) body)
            currentTerm res root

        let etaReduction src left root =
            ILogger.TryLog (logger, Detailed <| EtaReduction (src, left))
            let res = left
            currentTerm res root

        let betaReduction src body var sub root =
            ILogger.TryLog (logger, Detailed <| BetaReduction (src, body, var, sub))
            let res = substitute var sub body
            currentTerm res root

        // ============= Reduction ============================================

        let reduce term =
            let rec reduce' curStrategy term root =

                let inline tryBetaReduction term body var sub root =
                    maybeContinuation {
                        match reductionLimit with
                        | Some limit when reductionCount >= limit -> yield $"Exceeded the reduction limit: {limit}."
                        | _ ->
                            reductionCount <- reductionCount + 1
                            return betaReduction term body var sub root
                    }

                let inline reduceVar curStrategy term var boundValue root =
                    maybeContinuation {
                        match boundValue with
                        | Some boundValue -> return! reduce' curStrategy (expandVar var boundValue root) root
                        | _ -> return term
                    }

                let inline reduceNum num root =
                    maybeContinuation { return! reduce' curStrategy (expandNum num root) root }

                let inline reduceAbs (Strategy (_, Strong strong) as curStrategy) term var alpha body root =
                    maybeContinuation {
                        match alpha, body with

                        | _, Application (left, right) when
                            etaConversions
                            && right = irVar var
                            && not <| Set.contains var (freeVars false left)
                            ->
                            return! reduce' curStrategy (etaReduction term left root) root

                        | Some alpha, body when strong ->
                            return! reduce' curStrategy (alphaConversion term var alpha body root) root

                        | _, body when strong ->
                            let! reducedBody = reduce' curStrategy body (fun body -> irAbs var body |> root)
                            return irAbs var reducedBody

                        | _ -> return term
                    }

                let rec reduceApp (Strategy (Strict strict, _) as curStrategy) finish term left right root =
                    maybeContinuation {
                        match left with

                        | Abstraction (var, _, body) when finish || not strict ->
                            let! reducedTerm = tryBetaReduction term body var right root
                            return! reduce' curStrategy reducedTerm root

                        | _ when finish && strict -> return irApp left right

                        | _ when finish ->
                            let! reducedRight = reduce' strategy right (fun right -> irApp left right |> root)
                            return irApp left reducedRight

                        | _ when strict ->
                            let! reducedLeft = reduce' curStrategy left (fun left -> irApp left right |> root)
                            let! reducedRight = reduce' curStrategy right (fun right -> irApp reducedLeft right |> root)
                            let partial = irApp reducedLeft reducedRight
                            return! reduceApp curStrategy true partial reducedLeft reducedRight root

                        | _ ->
                            let! reducedLeft = reduce' callByName left (fun left -> irApp left right |> root)
                            let partial = irApp reducedLeft right
                            return! reduceApp curStrategy true partial reducedLeft right root
                    }

                match currentlyReducing term with
                | Variable (var, boundValue) -> reduceVar curStrategy term var boundValue root
                | Numeral num -> reduceNum num root
                | Abstraction (var, alpha, body) -> reduceAbs curStrategy term var alpha body root
                | Application (left, right) -> reduceApp curStrategy false term left right root in

            reduce' strategy term id id

        // ============= Expanding remaining variables ========================

        let finalize term =
            let rec finalize' term root =
                continuation {
                    match term with
                    | Variable (var, None) -> return lamVar var
                    | Variable (var, Some boundValue) -> return! finalize' (expandVar var boundValue root) root
                    | Numeral num -> return! finalize' (expandNum num root) root
                    | Abstraction (var, _, Application (left, right)) when
                        etaConversions
                        && right = irVar var
                        && not <| Set.contains var (freeVars false left)
                        ->
                        return! finalize' (etaReduction term left root) root
                    | Abstraction (var, None, body) ->
                        let! finalBody = finalize' body (fun body -> irAbs var body |> root)
                        return lamAbs var finalBody
                    | Abstraction (x, Some y, body) -> return! finalize' (alphaConversion term x y body root) root
                    | Application (left, right) ->
                        let! finalLeft = finalize' left (fun left -> irApp left right |> root)
                        let! finalRight = finalize' right (fun right -> irApp (toIR finalLeft) right |> root)
                        return lamApp finalLeft finalRight
                } in

            finalize' term id id

        // ============= Members ==============================================

        member _.Scope = List.rev bindings

        member this.AddBinding (var, value) =
            let boundValue = bindMany (this.Scope |> Map.ofList) value
            bindings <- (var, boundValue) :: bindings |> List.distinctBy fst

        member _.Reset () = bindings <- []

        member this.Reduce term =
            reductionCount <- 0

            term
            |> bindMany (this.Scope |> Map.ofList)
            |> reduce
            |> function
                | Ok reducedTerm -> Success (finalize reducedTerm, reductionCount)
                | Error msg -> Failure msg

        interface IReducer with
            member this.Scope = this.Scope
            member this.AddBinding (var, term) = this.AddBinding (var, term)
            member this.Reset () = this.Reset ()
            member this.Reduce term = this.Reduce term
