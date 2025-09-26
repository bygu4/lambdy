namespace Lambdy.Lib

open Lambdy.Lib.ParseTree
open Lambdy.Lib.LambdaTerm
open Lambdy.Lib.IR
open Lambdy.Lib.Interfaces

module Reducer =

    /// Class performing lambda term reduction.
    /// Use `verbose` option to print logs to the console.
    type Reducer(logger : ILogger, maxDepth : int) =
        let mutable bindings : (VariableName * IR) list = []

        /// Get free variables of the given lambda `term` using CPS with the given `cont`.
        [<TailCall>]
        let rec freeVars' strict ir cont =
            match ir with
            | Variable (var, Some boundValue) when strict ->
                freeVars' strict boundValue (fun valueVars -> Set.add var valueVars |> cont)
            | Variable (var, _) -> set [ var ] |> cont
            | Abstraction (var, _, term) -> freeVars' strict term (fun termVars -> Set.remove var termVars |> cont)
            | Application (left, right) ->
                freeVars'
                    strict
                    left
                    (fun leftVars -> freeVars' strict right (fun rightVars -> Set.union leftVars rightVars |> cont))

        /// Get free variables of the given lambda `term`.
        let freeVars strict ir = freeVars' strict ir id

        /// Get a variable, starting with `prefix`, that is not in `freeVars`.
        [<TailCall>]
        let rec nextFreeVar (Name prefix as var) freeVars =
            if not (Set.contains var freeVars) then
                var
            else
                nextFreeVar (Name <| prefix + "'") freeVars

        [<TailCall>]
        let rec bindMany' bindings ir cont =
            match ir with

            | _ when Map.isEmpty bindings -> cont ir

            | Variable (x, _) -> Variable (x, Map.tryFind x bindings) |> cont

            | Abstraction (x, _, term) ->
                let bindings = Map.remove x bindings

                if Map.isEmpty bindings then
                    cont ir
                else

                let vars = set <| Map.keys bindings
                let freeVarsT = freeVars false term

                let freeVarsS =
                    Set.intersect vars freeVarsT
                    |> Seq.map (fun var -> bindings.[var] |> freeVars true)
                    |> Set.unionMany

                let conv =
                    if Set.contains x freeVarsS then
                        Some <| nextFreeVar x (Set.union freeVarsT freeVarsS)
                    else
                        None

                bindMany' bindings term (fun boundTerm -> Abstraction (x, conv, boundTerm) |> cont)

            | Application (left, right) ->
                bindMany'
                    bindings
                    left
                    (fun boundLeft ->
                        bindMany' bindings right (fun boundRight -> Application (boundLeft, boundRight) |> cont)
                    )

        let bindMany bindings ir = bindMany' bindings ir id

        /// Substitute free occurrences of variable `var` in `term` with given term `sub`.
        /// Use CPS with the given `cont`.
        [<TailCall>]
        let rec substitute' var sub ir cont =
            match ir with

            // Substitute free occurrence of a matching variable.
            | Variable (x, _) when x = var -> cont sub

            // Obtain free variables of term to make substitution in and term to substitute with.
            // Perform alpha-conversion if necessary to avoid capturing, then make a substitution.
            | Abstraction (x, conv, term) when x <> var ->
                let freeVarsT = freeVars false term
                let freeVarsS = freeVars true sub

                let willSubstitute = Set.contains var freeVarsT
                let capturedX = Set.contains x freeVarsS

                let capturedConv =
                    match conv with
                    | Some y -> Set.contains y freeVarsS
                    | None -> false

                if willSubstitute then
                    let freeVarsT = freeVars true term
                    let y = nextFreeVar x (Set.union freeVarsT freeVarsS)

                    if capturedX then
                        substitute'
                            x
                            (Variable (y, None))
                            term
                            (fun convTerm ->
                                substitute' var sub convTerm (fun subsTerm -> Abstraction (y, None, subsTerm) |> cont)
                            )
                    else
                        let conv = if capturedConv then Some y else conv
                        substitute' var sub term (fun subsTerm -> Abstraction (x, conv, subsTerm) |> cont)

                else
                    cont ir

            // Make substitution in both parts of the application.
            | Application (left, right) ->
                substitute'
                    var
                    sub
                    left
                    (fun subsLeft ->
                        substitute' var sub right (fun subsRight -> Application (subsLeft, subsRight) |> cont)
                    )

            // Nothing to substitute, call continuation.
            | _ -> cont ir

        /// Substitute free occurrences of variable `var` in `term` with given term `sub`.
        /// Perform alpha-conversion if necessary.
        let substitute var sub ir = substitute' var sub ir id

        let substitution var boundValue =
            logger.Log <| Transformation (Substitution (var, toLambdaTerm boundValue))
            boundValue

        let alphaConversion src term x y =
            logger.Log <| Transformation (AlphaConversion (toLambdaTerm src, x, y))
            Abstraction (y, None, substitute x (Variable (y, None)) term)

        let betaReduction src term var sub =
            logger.Log
            <| Transformation (BetaReduction (toLambdaTerm src, toLambdaTerm term, var, toLambdaTerm sub))

            substitute var sub term

        let maybe step =
            function
            | Ok result -> step result
            | _ as err -> err

        let maybePassTo cont step = maybe step >> cont

        /// Reduce `term` in CPS using the given `cont`.
        /// Keep track of `depth` of the reduction and throw exception in case of exceeding.
        [<TailCall>]
        let rec reduce' ret depth ir root cont =
            if depth > maxDepth then
                cont <| Error "Max recursion depth exceeded during the reduction."
            else

            match ir with

            // Substitute with bound value and then reduce.
            | Variable (var, Some boundValue) -> reduce' ret depth (substitution var boundValue) root cont

            | Abstraction (x, Some y, term) when not ret -> reduce' ret depth (alphaConversion ir term x y) root cont

            // Reduce body of the abstraction and pass it to continuation.
            | Abstraction (var, None, term) when not ret ->
                reduce'
                    ret
                    (depth + 1)
                    term
                    (fun term -> Abstraction (var, None, term) |> root)
                    (maybePassTo cont (fun reducedTerm -> Abstraction (var, None, reducedTerm) |> Ok))

            // Perform beta-reduction, then check whether the resulting term is an abstraction.
            // Return if necessary to reduce the outermost redex, otherwise reduce the result.
            | Application (Abstraction (var, _, term), sub) ->
                reduce' ret depth (betaReduction ir term var sub) root cont

            // Reduce left part of the application first, then check for redex.
            // Perform beta-reduction if left part was reduced to an abstraction, otherwise reduce the right part.
            | Application (left, right) ->
                reduce'
                    true
                    (depth + 1)
                    left
                    (fun left -> Application (left, right))
                    (maybePassTo
                        cont
                        (fun reducedLeft ->
                            match reducedLeft with
                            | Abstraction _ -> reduce' ret depth (Application (reducedLeft, right)) root cont
                            | _ ->
                                reduce'
                                    false
                                    (depth + 1)
                                    right
                                    (fun right -> Application (reducedLeft, right) |> root)
                                    (maybePassTo
                                        cont
                                        (fun reducedRight -> Application (reducedLeft, reducedRight) |> Ok))
                        ))

            // Nothing to reduce, call continuation.
            | _ -> cont <| Ok ir

        /// Perform beta-reduction of the given lambda `term`.
        /// Perform alpha-conversion if necessary.
        let reduce ir = reduce' false 0 ir id id

        [<TailCall>]
        let rec closure' ir cont =
            match ir with
            | Variable (var, None) -> LambdaTerm.Variable var |> cont
            | Variable (var, Some boundValue) -> closure' (substitution var boundValue) cont
            | Abstraction (var, None, term) ->
                closure' term (fun termClosure -> LambdaTerm.Abstraction (var, termClosure) |> cont)
            | Abstraction (x, Some y, term) -> closure' (alphaConversion ir term x y) cont
            | Application (left, right) ->
                closure'
                    left
                    (fun leftClosure ->
                        closure' right (fun rightClosure -> LambdaTerm.Application (leftClosure, rightClosure) |> cont)
                    )

        let closure ir = closure' ir id

        /// Print defined variables to the console in order of addition.
        member _.Bindings = List.map (fun (var, value) -> var, toLambdaTerm value) bindings

        /// Perform beta-reduction of the given lambda `term` according to defined variables.
        /// Perform alpha-conversion if necessary.
        member _.Reduce term =
            term
            |> toIR
            |> bindMany (Map.ofList bindings)
            |> reduce
            |> function
                | Ok result -> closure result |> Ok
                | Error msg -> Error msg

        /// Define a `var` to be substituted with the given `term`.
        member _.AddBinding (var : VariableName, value : LambdaTerm) =
            let boundValue = toIR value |> bindMany (Map.ofList bindings)
            bindings <- (var, boundValue) :: bindings

        /// Reset defined variables.
        member _.ResetBindings () = bindings <- []

        interface IReducer with
            member this.Bindings = this.Bindings
            member this.Reduce term = this.Reduce term
            member this.AddBinding (var, term) = this.AddBinding (var, term)
            member this.ResetBindings () = this.ResetBindings ()
