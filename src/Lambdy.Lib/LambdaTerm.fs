namespace Lambdy.Lib

open Lambdy.Lib.ParseTree
open Lambdy.Lib.IR

// Module containing finalized lambda term definitions.
module LambdaTerm =

    /// The finalized lambda term representation.
    type LambdaTerm =
        | Variable of name : VariableName
        | Abstraction of variable : VariableName * body : LambdaTerm
        | Application of left : LambdaTerm * right : LambdaTerm

    // ============= Constructors =========================================

    let inline lamVar name = Variable name
    let inline lamAbs var body = Abstraction (var, body)
    let inline lamApp left right = Application (left, right)

    // ============= LambdaTerm conversions ===============================

    [<TailCall>]
    let rec private toIR' term cont =
        match term with
        | Variable var -> irVar var |> cont
        | Abstraction (var, body) -> toIR' body (fun bodyIR -> irAbs var bodyIR |> cont)
        | Application (left, right) ->
            toIR' left (fun leftIR -> toIR' right (fun rightIR -> irApp leftIR rightIR |> cont))

    let toIR term = toIR' term id

    [<TailCall>]
    let rec private toLambdaTerm' expand ir subs cont =
        match ir with
        | IR.Variable (_, Some boundValue) when expand -> toLambdaTerm' expand boundValue Map.empty cont
        | IR.Variable (x, _) ->
            match Map.tryFind x subs with
            | Some y -> lamVar y |> cont
            | None -> lamVar x |> cont
        | Numeral num when expand -> toLambdaTerm' expand (churchNumeral num) subs cont
        | Numeral num -> num.ToString () |> Name |> lamVar |> cont
        | IR.Abstraction (x, Some y, body) when expand ->
            toLambdaTerm' expand body (Map.add x y subs) (fun bodyTerm -> lamAbs y bodyTerm |> cont)
        | IR.Abstraction (var, _, body) -> toLambdaTerm' expand body subs (fun bodyTerm -> lamAbs var bodyTerm |> cont)
        | IR.Application (left, right) ->
            toLambdaTerm'
                expand
                left
                subs
                (fun leftTerm -> toLambdaTerm' expand right subs (fun rightTerm -> lamApp leftTerm rightTerm |> cont))

    let toLambdaTerm expand ir = toLambdaTerm' expand ir Map.empty id

    let lamToString strict parenthesize = toIR >> irToString strict parenthesize

    // ============= Alpha equivalence ====================================

    [<TailCall>]
    let rec private areAlphaEquiv' first second subs cont =
        match first, second with
        | Variable x, Variable y ->
            match Map.tryFind y subs with
            | Some z -> x = z |> cont
            | None -> x = y |> cont
        | Abstraction (var1, body1), Abstraction (var2, body2) when var1 = var2 -> areAlphaEquiv' body1 body2 subs cont
        | Abstraction (var1, body1), Abstraction (var2, body2) ->
            areAlphaEquiv' body1 body2 (Map.add var2 var1 subs) cont
        | Application (left1, right1), Application (left2, right2) ->
            areAlphaEquiv'
                left1
                left2
                subs
                (function
                | true -> areAlphaEquiv' right1 right2 subs cont
                | false -> cont false
                )
        | _ -> cont false

    let areAlphaEquiv first second =
        areAlphaEquiv' first second Map.empty id

    [<TailCall>]
    let rec private alphaEquivNum' name1 name2 term acc =
        match term with
        | Variable var when var = name2 -> Some acc
        | Application (left, right) when left = lamVar name1 -> alphaEquivNum' name1 name2 right (acc + 1)
        | _ -> None

    let alphaEquivNum term =
        match term with
        | Abstraction (name1, Abstraction (name2, body)) -> alphaEquivNum' name1 name2 body 0
        | _ -> None

    [<TailCall>]
    let rec private findAlphaEquiv' scope term cont =

        let inline findEquivVar scope term =
            Seq.tryFind (fun (_, value) -> areAlphaEquiv term value) scope
            |> function
                | Some (name, _) -> lamVar name |> Some
                | None ->
                    alphaEquivNum term
                    |> function
                        | Some num -> num.ToString () |> Name |> lamVar |> Some
                        | None -> None

        match term with
        | Variable _ -> cont None
        | Abstraction (var, body) ->
            match findEquivVar scope term with
            | Some _ as equiv -> cont equiv
            | None ->
                findAlphaEquiv'
                    scope
                    body
                    (function
                    | Some equivBody -> lamAbs var equivBody |> Some |> cont
                    | None -> cont None
                    )
        | Application (left, right) ->
            match findEquivVar scope term with
            | Some _ as equiv -> cont equiv
            | None ->
                findAlphaEquiv'
                    scope
                    left
                    (function
                    | Some equivLeft ->
                        findAlphaEquiv'
                            scope
                            right
                            (function
                            | Some equivRight -> lamApp equivLeft equivRight |> Some |> cont
                            | None -> lamApp equivLeft right |> Some |> cont
                            )
                    | None ->
                        findAlphaEquiv'
                            scope
                            right
                            (function
                            | Some equivRight -> lamApp left equivRight |> Some |> cont
                            | None -> cont None
                            )
                    )

    let findAlphaEquiv scope term = findAlphaEquiv' scope term id
