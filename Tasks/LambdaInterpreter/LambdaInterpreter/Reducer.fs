namespace LambdaInterpreter

open System

open LambdaInterpreter.Syntax
open AST

/// Class performing lambda term reduction.
/// Use `verbose` option to print logs to the console.
type Reducer (?verbose: bool) =
    let logger = new Logger (?verbose=verbose)
    let mutable variables: (Variable * LambdaTerm) list = []

    /// Max allowed depth of recursion during the term reduction.
    [<Literal>]
    let MaxDepth = 1000000

    /// Message to fail with in case of max recursion depth exceeding.
    [<Literal>]
    let MaxDepthExceededMessage = "Error: max recursion depth exceeded during the reduction."

    /// Get free variables of the given lambda `term`.
    let rec freeVars term =
        match term with
        | Variable (Name v) -> set [v]
        | Abstraction (Name v, term) -> Set.remove v (freeVars term)
        | Application (left, right) -> 
            Set.union (freeVars left) (freeVars right)

    /// Get a variable, starting with `prefix`, that is not in `freeVars`.
    let rec nextFreeVar prefix freeVars =
        if not (Set.contains prefix freeVars) then prefix
        else nextFreeVar (prefix + "'") freeVars

    /// Substitute free occurrences of variable `var` in `term` with given term `sub`.
    /// Perform alpha-conversion if necessary.
    let rec substitute term (Name var, sub) =
        match term with
        | Variable (Name x) when x = var ->
            logger.Log <| Substitution (Name var, sub)
            sub
        | Variable _ as var -> var
        | Abstraction (Name x, _) as abs when x = var -> abs
        | Abstraction (Name x, term) ->
            let freeVarsS = freeVars sub
            let freeVarsT = freeVars term
            if not (Set.contains var freeVarsT && Set.contains x freeVarsS) then
                Abstraction (Name x, substitute term (Name var, sub))
            else
                let y = nextFreeVar x (Set.union freeVarsS freeVarsT) |> Name
                logger.Log <| AlphaConversion (Name x, y)
                Abstraction (y, substitute (substitute term (Name x, Variable y)) (Name var, sub))
        | Application (left, right) ->
            Application (substitute left (Name var, sub), substitute right (Name var, sub))

    /// Substitute variables in `term` according to the given `subs` pair sequence.
    let substituteMany term subs =
        subs |> Seq.fold substitute term

    /// Perform beta-reduction of the given lambda `term`.
    /// Perform alpha-conversion if necessary.
    let reduce term =

        /// Reduce `term` in CPS using the given `cont`.
        /// Keep track of `depth` of the reduction and throw exception in case of exceeding.
        let rec reduce term depth cont =
            if depth > MaxDepth then raise <| StackOverflowException MaxDepthExceededMessage
            logger.Log <| Reducing term
            match term with

            // Nothing to reduce, call continuation.
            | Variable _ as var -> cont var

            // Reduce the right part of the abstraction and pass it to continuation.
            | Abstraction (var, term) ->
                reduce term (depth + 1) (
                    fun reducedTerm -> Abstraction (var, reducedTerm) |> cont
                )

            // Perform beta reduction and then reduce the result.
            | Application (Abstraction (var, term) as abs, sub) as source ->
                logger.Log <| BetaReduction (source, term, var, sub)
                let term = substitute term (var, sub)
                if term <> source then reduce term depth cont
                else
                    logger.Log <| UnableToReduce term
                    reduce abs (depth + 1) (fun reducedAbs ->
                        reduce sub (depth + 1) (fun reducedSub ->
                            Application (reducedAbs, reducedSub) |> cont
                        )
                    )

            // Reduce both parts of the application and pass it to continuation.
            | Application (left, right) ->
                reduce left (depth + 1) (fun reducedLeft ->
                    reduce right (depth + 1) (fun reducedRight ->
                        match reducedLeft with
                        | Abstraction _ -> reduce (Application (reducedLeft, reducedRight)) depth cont
                        | _ -> Application (reducedLeft, reducedRight) |> cont
                    )
                )

        in reduce term 0 id

    /// Define a `var` to be substituted with the given `term`.
    member _.AddDefinition (var: Variable, term: LambdaTerm) =
        variables <- (var, term) :: variables
        logger.Log <| NewDefinition (var, term)

    /// Reset defined variables.
    member _.ResetDefinitions () =
        variables <- []
        logger.Log <| DefinitionsReset

    /// Print defined variables to the console in order of addition.
    member _.DisplayDefinitions () =

        /// Print the given definition of `var` with `term` to the console.
        let printDefinition (Name var, term) =
            printfn $"-  {var} := {toString term}"

        logger.Log <| DisplayingDefinitions
        if variables.IsEmpty then logger.Log <| NoVariablesDefined else
        variables
        |> List.rev
        |> List.distinct
        |> List.map printDefinition
        |> ignore

    /// Perform beta-reduction of the given lambda `term` according to defined variables.
    /// Perform alpha-conversion if necessary.
    member _.Reduce (term: LambdaTerm): LambdaTerm =
        logger.Log <| StartedReducing term
        let result = variables |> substituteMany term |> reduce
        logger.Log <| DoneReducing
        result
