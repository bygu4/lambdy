namespace LambdaInterpreter

open AST

/// Log record of reducer execution.
type LogRecord =
    | AlphaConversion of Variable * Variable
    | BetaReduction of LambdaTerm * LambdaTerm * Variable * LambdaTerm
    | Substitution of Variable * LambdaTerm
    | StartedReducing of LambdaTerm
    | Reducing of LambdaTerm
    | AddingDefinition of Variable * LambdaTerm
    | Resetting

/// Class performing lambda term reduction.
/// Use `verbose` option to print logs to the console.
type Reducer (?verbose: bool) =
    let verbose = defaultArg verbose false
    let mutable variables = new Map<Variable, LambdaTerm> ([])

    /// Print a log message from the given `record` according to verbosity.
    let log record =
        if verbose then
            match record with
            | AlphaConversion (Name x, Name y) ->
                $"  [alpha-conversion]: {x} -> {y}"
            | BetaReduction (source, term, Name var, sub) ->
                $"  [beta-reduction]: {toString source} -> {toStringWithBrackets term}[{var} := {toString sub}]"
            | Substitution (Name var, sub) ->
                $"  [substitution]: {var} -> {toString sub}"
            | StartedReducing term -> toString term
            | Reducing term -> $"  reducing {toString term} ..."
            | AddingDefinition (Name var, term) -> $"adding definition: {var} = {toString term} ..."
            | Resetting -> "resetting defined variables ..."
            |> printfn "%s"

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
    let rec substitute term (Name var) sub =
        match term with
        | Variable (Name x) when x = var ->
            log <| Substitution (Name var, sub)
            sub
        | Variable _ as var -> var
        | Abstraction (Name x, _) as abs when x = var -> abs
        | Abstraction (Name x, term) ->
            let freeVarsS = freeVars sub
            let freeVarsT = freeVars term
            if not (Set.contains var freeVarsT && Set.contains x freeVarsS) then
                Abstraction (Name x, substitute term (Name var) sub)
            else
                let y = nextFreeVar x (Set.union freeVarsS freeVarsT) |> Name
                log <| AlphaConversion (Name x, y)
                Abstraction (y, substitute (substitute term (Name x) (Variable y)) (Name var) sub)
        | Application (left, right) ->
            Application (substitute left (Name var) sub, substitute right (Name var) sub)

    /// Substitute variables in `term` according to the `subs` mapping.
    let substituteMany term (subs: Map<Variable, LambdaTerm>) =
        subs
        |> Seq.fold (fun acc pair -> substitute acc pair.Key pair.Value) term

    /// Perform beta-reduction of the given lambda `term`.
    /// Perform alpha-conversion if necessary.
    let rec reduce term =
        log <| Reducing term
        match term with
        | Variable _ as var -> var
        | Abstraction (x, term) -> Abstraction (x, reduce term)
        | Application (Abstraction (var, term) as abs, sub) as source ->
            log <| BetaReduction (source, term, var, sub)
            let term = substitute term var sub
            if term <> source then reduce term
            else Application (reduce abs, reduce sub)
        | Application (left, right) ->
            let left = reduce left
            let right = reduce right
            match left with
            | Abstraction _ -> reduce (Application (left, right))
            | _ -> Application (left, right)

    /// Define a `var` to be substituted with the given `term`.
    member _.AddDefinition (var: Variable, term: LambdaTerm) =
        log <| AddingDefinition (var, term)
        variables <- variables.Add (var, substituteMany term variables)
        if verbose then printf "\n"

    /// Reset defined variables.
    member _.Reset () =
        log <| Resetting
        variables <- new Map<Variable, LambdaTerm> ([])
        if verbose then printf "\n"

    /// Perform beta-reduction of the given lambda `term` according to defined variables.
    /// Perform alpha-conversion if necessary.
    member _.Reduce (term: LambdaTerm): LambdaTerm =
        log <| StartedReducing term
        variables |> substituteMany term |> reduce
