namespace LambdaInterpreter

open System

open LambdaInterpreter.Syntax
open AST

/// Class performing lambda term reduction.
/// Use `verbose` option to print logs to the console.
type Reducer(?verbose: bool) =
    let logger = new Logger(?verbose = verbose)
    let mutable variables: (Variable * LambdaTerm) list = []

    /// Max allowed depth of recursion during the term reduction.
    [<Literal>]
    let MaxDepth = 1000000

    /// Message to fail with in case of max recursion depth exceeding.
    [<Literal>]
    let MaxDepthExceededMessage =
        "Error: max recursion depth exceeded during the reduction."

    /// Get free variables of the given lambda `term` using CPS with the given `cont`.
    [<TailCall>]
    let rec freeVarsCPS term cont =
        match term with
        | Variable(Name v) -> set [ v ] |> cont
        | Abstraction(Name v, term) -> freeVarsCPS term (fun termVars -> Set.remove v termVars |> cont)
        | Application(left, right) ->
            freeVarsCPS left (fun leftVars -> freeVarsCPS right (fun rightVars -> Set.union leftVars rightVars |> cont))

    /// Get free variables of the given lambda `term`.
    let freeVars term = freeVarsCPS term id

    /// Get a variable, starting with `prefix`, that is not in `freeVars`.
    let nextFreeVar prefix freeVars =
        let rec nextFreeVar prefix freeVars =
            if not (Set.contains prefix freeVars) then
                prefix
            else
                nextFreeVar (prefix + "'") freeVars in

        nextFreeVar prefix freeVars

    /// Substitute free occurrences of variable `var` in `term` with given term `sub`.
    /// Use CPS with the given `cont`.
    [<TailCall>]
    let rec substituteCPS term (Name var, sub) cont =
        match term with

        // Substitute free occurrence of a matching variable.
        | Variable(Name x) when x = var ->
            logger.Log <| Substitution(Name var, sub)
            cont sub

        // Nothing to substitute, call continuation.
        | Variable _ as var -> cont var

        // Our variable is bounded, call continuation.
        | Abstraction(Name x, _) as abs when x = var -> cont abs

        // Obtain free variables of term to make substitution in and term to substitute with.
        // Perform alpha-conversion if necessary to avoid capturing, then make a substitution.
        | Abstraction(Name x, term) ->
            let freeVarsT = freeVars term
            let freeVarsS = freeVars sub

            if Set.contains var freeVarsT && Set.contains x freeVarsS then
                let y = nextFreeVar x (Set.union freeVarsT freeVarsS) |> Name
                logger.Log <| AlphaConversion(Name x, y)

                substituteCPS term (Name x, Variable y) (fun converted ->
                    substituteCPS converted (Name var, sub) (fun subsTerm -> Abstraction(y, subsTerm) |> cont))
            else
                substituteCPS term (Name var, sub) (fun subsTerm -> Abstraction(Name x, subsTerm) |> cont)

        // Make substitution in both parts of the application.
        | Application(left, right) ->
            substituteCPS left (Name var, sub) (fun subsLeft ->
                substituteCPS right (Name var, sub) (fun subsRight -> Application(subsLeft, subsRight) |> cont))

    /// Substitute free occurrences of variable `var` in `term` with given term `sub`.
    /// Perform alpha-conversion if necessary.
    let substitute term (Name var, sub) = substituteCPS term (Name var, sub) id

    /// Substitute variables in `term` according to the given `subs` pair sequence.
    let substituteMany term subs = subs |> Seq.fold substitute term

    /// Reduce `term` in CPS using the given `cont`.
    /// Keep track of `depth` of the reduction and throw exception in case of exceeding.
    [<TailCall>]
    let rec reduceCPS term depth ret cont =
        if depth > MaxDepth then
            raise <| StackOverflowException MaxDepthExceededMessage

        logger.Log <| Reducing term

        match term with

        // Nothing to reduce, call continuation.
        | Variable _ as var -> cont var

        // Reduce the right part of the abstraction and pass it to continuation.
        | Abstraction(var, term) ->
            reduceCPS term (depth + 1) false (fun reducedTerm -> Abstraction(var, reducedTerm) |> cont)

        // Perform beta-reduction, then check whether the resulting term is an abstraction.
        // Return if necessary to reduce the outermost redex, otherwise reduce the result.
        | Application(Abstraction(var, term) as abs, sub) as source ->
            logger.Log <| BetaReduction(source, term, var, sub)
            let term = substitute term (var, sub)

            match term with
            | Abstraction _ when ret -> cont term
            | _ ->
                if term <> source then
                    reduceCPS term depth ret cont
                else
                    logger.Log <| UnableToReduce term

                    reduceCPS abs (depth + 1) false (fun reducedAbs ->
                        reduceCPS sub (depth + 1) false (fun reducedSub -> Application(reducedAbs, reducedSub) |> cont))

        // Reduce left part of the application first, then check for redex.
        // Perform beta-reduction if left part was reduced to an abstraction, otherwise reduce the right part.
        | Application(left, right) ->
            reduceCPS left (depth + 1) true (fun reducedLeft ->
                match reducedLeft with
                | Abstraction _ -> reduceCPS (Application(reducedLeft, right)) depth ret cont
                | _ ->
                    reduceCPS right (depth + 1) false (fun reducedRight ->
                        Application(reducedLeft, reducedRight) |> cont))

    /// Perform beta-reduction of the given lambda `term`.
    /// Perform alpha-conversion if necessary.
    let reduce term = reduceCPS term 0 false id

    /// Perform beta-reduction of the given lambda `term` according to defined variables.
    /// Perform alpha-conversion if necessary.
    member _.Reduce(term: LambdaTerm) : LambdaTerm =
        logger.Log <| StartedReducing term
        let result = variables |> substituteMany term |> reduce
        logger.Log <| DoneReducing
        result

    /// Define a `var` to be substituted with the given `term`.
    member _.AddDefinition(var: Variable, term: LambdaTerm) =
        variables <- (var, term) :: variables
        logger.Log <| NewDefinition(var, term)

    /// Reset defined variables.
    member _.ResetDefinitions() =
        variables <- []
        logger.Log <| DefinitionsReset

    /// Print defined variables to the console in order of addition.
    member _.DisplayDefinitions() =

        /// Print the given definition of `var` with `term` to the console.
        let printDefinition (Name var, term) = printfn $"-  {var} := {toString term}"

        logger.Log <| DisplayingDefinitions

        if variables.IsEmpty then
            logger.Log <| NoVariablesDefined
        else
            variables |> List.rev |> List.distinct |> List.map printDefinition |> ignore
