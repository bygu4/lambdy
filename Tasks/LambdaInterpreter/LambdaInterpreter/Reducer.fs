namespace LambdaInterpreter

open System
open AST

/// Log record of reducer execution.
type LogRecord =
    | StartedReducing of LambdaTerm
    | DoneReducing
    | Reducing of LambdaTerm
    | AlphaConversion of Variable * Variable
    | BetaReduction of LambdaTerm * LambdaTerm * Variable * LambdaTerm
    | Substitution of Variable * LambdaTerm
    | UnableToReduce of LambdaTerm
    | NewDefinition of Variable * LambdaTerm
    | DefinitionsReset

/// Class performing lambda term reduction.
/// Use `verbose` option to print logs to the console.
type Reducer (?verbose: bool) =
    let verbose = defaultArg verbose false
    let mutable variables: (Variable * LambdaTerm) list = []

    /// Max allowed depth of recursion during the term reduction.
    [<Literal>]
    let MaxRecursionDepth = 4096

    /// Message to fail with in case of stack overflow.
    [<Literal>]
    let StackOverflowMessage = "Error: max recursion depth exceeded during the reduction."

    /// Print a log message from the given `record` according to verbosity.
    let log record =
        if verbose then
            match record with
            | StartedReducing term -> $"{toString term}\n|"
            | DoneReducing -> "V"
            | Reducing term ->
                $"|  reducing {toString term} ..."
            | AlphaConversion (Name x, Name y) ->
                $"|  [alpha-conversion]: {x} -> {y}"
            | BetaReduction (source, term, Name var, sub) ->
                $"|  [beta-reduction]: {toString source} -> {toStringWithBrackets term}[{var} := {toString sub}]"
            | Substitution (Name var, sub) ->
                $"|  [substitution]: {var} -> {toString sub}"
            | UnableToReduce term ->
                $"|  unable to reduce {toString term}\n|  term didn't change after beta-reduction"
            | NewDefinition (Name var, term) -> $"(!) definition: {var} = {toString term}"
            | DefinitionsReset -> "(!) definitions were reset"
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

    /// Substitute variables in `term` according to the given `subs` pair sequence.
    let substituteMany term subs =
        subs
        |> Seq.fold (fun acc (var, sub) -> substitute acc var sub) term

    /// Perform beta-reduction of the given lambda `term`.
    /// Perform alpha-conversion if necessary.
    let reduce term =
        let rec reduce term depth =
            if depth > MaxRecursionDepth then raise <| StackOverflowException StackOverflowMessage
            log <| Reducing term
            match term with
            | Variable _ as var -> var
            | Abstraction (x, term) -> Abstraction (x, reduce term (depth + 1))
            | Application (Abstraction (var, term) as abs, sub) as source ->
                log <| BetaReduction (source, term, var, sub)
                let term = substitute term var sub
                if term <> source then reduce term (depth + 1)
                else
                    log <| UnableToReduce term
                    Application (reduce abs (depth + 1), reduce sub (depth + 1))
            | Application (left, right) ->
                let left = reduce left (depth + 1)
                let right = reduce right (depth + 1)
                match left with
                | Abstraction _ -> reduce (Application (left, right)) (depth + 1)
                | _ -> Application (left, right)
        in reduce term 0

    /// Define a `var` to be substituted with the given `term`.
    member _.AddDefinition (var: Variable, term: LambdaTerm) =
        variables <- (var, term) :: variables
        log <| NewDefinition (var, term)

    /// Reset defined variables.
    member _.Reset () =
        variables <- []
        log <| DefinitionsReset

    /// Perform beta-reduction of the given lambda `term` according to defined variables.
    /// Perform alpha-conversion if necessary.
    member _.Reduce (term: LambdaTerm): LambdaTerm =
        log <| StartedReducing term
        let result = variables |> substituteMany term |> reduce
        log <| DoneReducing
        result
