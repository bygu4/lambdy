module LambdaTerm

type Variable = string

type LambdaTerm =
    | Variable of Variable
    | Abstraction of Variable * LambdaTerm
    | Application of LambdaTerm * LambdaTerm

let rec freeVars term =
    match term with
    | Variable v -> set [v]
    | Abstraction (v, term) -> Set.remove v (freeVars term)
    | Application (left, right)
        -> Set.union (freeVars left) (freeVars right)

let rec nextFreeVar var freeVars =
    if not (Set.contains var freeVars) then var
    else nextFreeVar (var + "'") freeVars

let rec substitute term var sub =
    match term with
    | Variable x when x = var -> sub
    | Variable _ as var -> var
    | Abstraction (x, _) as abs when x = var -> abs
    | Abstraction (x, term) ->
        let freeVarsS = freeVars sub
        let freeVarsT = freeVars term
        if not (Set.contains var freeVarsT && Set.contains x freeVarsS) then
            Abstraction (x, substitute term var sub)
        else
            let y = nextFreeVar x (Set.union freeVarsS freeVarsT)
            Abstraction (y, substitute (substitute term x (Variable y)) var sub)
    | Application (left, right) ->
        Application (substitute left var sub, substitute right var sub)

let rec reduce term =
    match term with
    | Variable _ as var -> var
    | Abstraction (x, term) -> Abstraction (x, reduce term)
    | Application (Abstraction (var, term), sub) ->
        reduce (substitute term var sub)
    | Application (left, right)
        -> Application (reduce left, reduce right)
