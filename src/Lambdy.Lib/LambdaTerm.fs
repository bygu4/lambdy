namespace Lambdy.Lib

open Lambdy.Lib.Continuation
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

    let toIR term =
        let rec toIR' term =
            continuation {
                match term with
                | Variable var -> return irVar var
                | Abstraction (var, body) ->
                    let! bodyIR = toIR' body
                    return irAbs var bodyIR
                | Application (left, right) ->
                    let! leftIR = toIR' left
                    let! rightIR = toIR' right
                    return irApp leftIR rightIR
            } in

        toIR' term id

    let toLambdaTerm expand ir =
        let rec toLambdaTerm' expand ir subs =
            continuation {
                match ir with
                | IR.Variable (_, Some boundValue) when expand -> return! toLambdaTerm' expand boundValue Map.empty
                | IR.Variable (x, _) ->
                    match Map.tryFind x subs with
                    | Some y -> return lamVar y
                    | None -> return lamVar x
                | Numeral num when expand -> return! toLambdaTerm' expand (churchNumeral num) subs
                | Numeral num -> return num.ToString () |> Name |> lamVar
                | IR.Abstraction (x, Some y, body) when expand ->
                    let! bodyTerm = toLambdaTerm' expand body (Map.add x y subs)
                    return lamAbs y bodyTerm
                | IR.Abstraction (var, _, body) ->
                    let! bodyTerm = toLambdaTerm' expand body subs
                    return lamAbs var bodyTerm
                | IR.Application (left, right) ->
                    let! leftTerm = toLambdaTerm' expand left subs
                    let! rightTerm = toLambdaTerm' expand right subs
                    return lamApp leftTerm rightTerm
            } in

        toLambdaTerm' expand ir Map.empty id

    let expand = toLambdaTerm true

    let lamToString strict parenthesize = toIR >> irToString strict parenthesize

    // ============= Alpha equivalence ====================================

    let areAlphaEquiv first second =
        let rec areAlphaEquiv' first second subs =
            continuation {
                match first, second with
                | Variable x, Variable y ->
                    match Map.tryFind y subs with
                    | Some z -> return x = z
                    | None -> return x = y
                | Abstraction (var1, body1), Abstraction (var2, body2) when var1 = var2 ->
                    return! areAlphaEquiv' body1 body2 subs
                | Abstraction (var1, body1), Abstraction (var2, body2) ->
                    return! areAlphaEquiv' body1 body2 (Map.add var2 var1 subs)
                | Application (left1, right1), Application (left2, right2) ->
                    let! equivLeft = areAlphaEquiv' left1 left2 subs

                    match equivLeft with
                    | true -> return! areAlphaEquiv' right1 right2 subs
                    | false -> return false
                | _ -> return false
            } in

        areAlphaEquiv' first second Map.empty id

    let alphaEquivNum term =
        let rec alphaEquivNum' name1 name2 term acc =
            match term with
            | Variable var when var = name2 -> Some acc
            | Application (left, right) when left = lamVar name1 -> alphaEquivNum' name1 name2 right (acc + 1)
            | _ -> None in

        match term with
        | Abstraction (name1, Abstraction (name2, body)) -> alphaEquivNum' name1 name2 body 0
        | _ -> None

    let findAlphaEquiv scope term =
        let rec findAlphaEquiv' scope term =

            let inline findEquivVar scope term =
                Seq.tryFind (fun (_, value) -> areAlphaEquiv term value) scope
                |> function
                    | Some (name, _) -> lamVar name |> Some
                    | None ->
                        alphaEquivNum term
                        |> function
                            | Some num -> num.ToString () |> Name |> lamVar |> Some
                            | None -> None

            continuation {
                match term with
                | Variable _ -> return None
                | Abstraction (var, body) ->
                    match findEquivVar scope term with
                    | Some _ as equiv -> return equiv
                    | None ->
                        let! equivBody = findAlphaEquiv' scope body

                        match equivBody with
                        | Some equivBody -> return lamAbs var equivBody |> Some
                        | None -> return None
                | Application (left, right) ->
                    match findEquivVar scope term with
                    | Some _ as equiv -> return equiv
                    | None ->
                        let! equivLeft = findAlphaEquiv' scope left
                        let! equivRight = findAlphaEquiv' scope right

                        match equivLeft, equivRight with
                        | Some equivLeft, Some equivRight -> return lamApp equivLeft equivRight |> Some
                        | Some equivLeft, None -> return lamApp equivLeft right |> Some
                        | None, Some equivRight -> return lamApp left equivRight |> Some
                        | _ -> return None
            } in

        findAlphaEquiv' scope term id
