namespace Lambdy.Tests

open System.Text.RegularExpressions

open NUnit.Framework
open FsUnit
open FsCheck.NUnit
open FsCheck.FSharp
open FParsec

open Lambdy.Lib.Literals
open Lambdy.Lib.ParseTree
open Lambdy.Lib.Parsers
open Lambdy.Lib.LambdaTerm
open Lambdy.Lib.AST

module LambdaTermTests =

    let lamVarTerm = Name >> lamVar
    let lamI name = lamAbs (Name name) (lamVarTerm name)
    let lamOmega = lamAbs (Name "x") (lamApp (lamVarTerm "x") (lamVarTerm "x"))

    // ============= Term to string conversion unit tests =================

    let toStringInputs =
        [
            lamVarTerm "x", false, false
            lamVarTerm "smth", false, true
            lamVarTerm "trololo", true, false
            lamApp (lamVarTerm "rerere") (lamVarTerm "bababa"), false, false
            lamApp (lamVarTerm "x") (lamVarTerm "y"), true, false
            lamApp (lamVarTerm "S") lamOmega, true, true
            lamAbs (Name "x") (lamApp (lamVarTerm "x") (lamVarTerm "y")), false, false
            lamAbs (Name "VAR_1") (lamVarTerm "VAR_2"), false, true
            lamAbs (Name "fst") (lamAbs (Name "snd") (lamApp (lamVarTerm "snd") (lamVarTerm "fst"))), true, true
            lamApp (lamApp (lamVarTerm "A") (lamVarTerm "B")) (lamVarTerm "C"), true, false
            lamApp (lamVarTerm "X") (lamApp (lamVarTerm "Y") (lamVarTerm "Z")), false, false
            lamAbs (Name "A1") (lamAbs (Name "A2") (lamAbs (Name "A3") (lamVarTerm "A4"))), false, false
            lamApp (lamI "x") (lamVarTerm "y"), false, false
            lamAbs (Name "U") (lamApp (lamVarTerm "V") (lamAbs (Name "W") (lamVarTerm "K"))), false, true
            lamApp (lamI "Y") (lamApp (lamApp (lamI "Y") (lamI "Y")) (lamI "Y")), false, false
            lamAbs (Name "k") (lamApp (lamApp (lamI "S") (lamI "S")) (lamI "S")), true, false
            lamAbs (Name "c") (lamApp (lamVarTerm "z") (lamAbs (Name "y") lamOmega)), false, false
            lamAbs (Name "x") (lamAbs (Name "y") (lamAbs (Name "z") (lamApp (lamI "x") lamOmega))), true, true
            lamApp
                (lamApp (lamApp (lamVarTerm "T") (lamVarTerm "K")) (lamVarTerm "P"))
                (lamApp (lamVarTerm "K") (lamVarTerm "K")),
            true,
            false
        ]

    let toStringResults =
        [
            "x"
            "smth"
            "trololo"
            "rerere bababa"
            "x y"
            "(S (\\x.(x x)))"
            "\\x.x y"
            "(\\VAR_1.VAR_2)"
            "(\\fst.(\\snd.(snd fst)))"
            "(A B) C"
            "X (Y Z)"
            "\\A1.\\A2.\\A3.A4"
            "(\\x.x) y"
            "(\\U.V \\W.K)"
            "(\\Y.Y) ((\\Y.Y) (\\Y.Y) \\Y.Y)"
            "\\k.(((\\S.S) (\\S.S)) (\\S.S))"
            "\\c.z \\y.\\x.x x"
            "(\\x.(\\y.(\\z.((\\x.x) (\\x.(x x))))))"
            "((T K) P) (K K)"
        ]

    let toStringTestCases =
        Seq.zip toStringInputs toStringResults
        |> Seq.map (fun ((x, y, z), t) -> TestCaseData (x, y, z, t))

    [<TestCaseSource(nameof (toStringTestCases))>]
    let testToString term strict parenthesize expectedStr =
        term |> lamToString strict parenthesize |> should equal expectedStr

    // ============= Lambda term conversions properties ===================

    // Arbitrary type for generating lambda terms, avoiding stack overflows.
    type GenTerm =
        static member LambdaTerm () =
            let rec genTerm size =

                let inline genName () =
                    let trim = fun (str : string) -> str.Trim ()

                    let isValidName =
                        fun str -> not <| isKeyword str && Regex.IsMatch (str, $"^{VariablePattern}$")

                    ArbMap.defaults
                    |> ArbMap.generate<string>
                    |> Gen.filter isValidName
                    |> Gen.map (trim >> Name)

                let inline genVar () = genName () |> Gen.map Variable

                let inline genAbs size =
                    Gen.map2 (fun var term -> lamAbs var term) (genName ()) (genTerm (size - 1))

                let inline genApp size =
                    Gen.map2 (fun left right -> lamApp left right) (genTerm (size / 2)) (genTerm (size / 2))

                match size with
                | 0 -> genVar ()
                | n when n > 0 -> Gen.oneof [ genVar () ; genAbs size ; genApp size ]
                | _ -> invalidArg (nameof size) "Term size must be non-negative"

            Gen.sized genTerm |> Arb.fromGen

    // For some reason this property is only properly checked when passing an additional argument :(.
    [<Property(Arbitrary = [| typeof<GenTerm> |])>]
    let ``Lambda term doesn't change after IR conversions`` initial (_ : bool) =
        initial |> toIR |> toLambdaTerm |> (=) initial

    [<Property(Arbitrary = [| typeof<GenTerm> |])>]
    let ``Lambda term is equal to its parsed string representation`` initial strict parenthesize =
        initial
        |> lamToString strict parenthesize
        |> run term
        |> function
            | Success (parseTree, _, _) -> parseTree |> buildTermAST |> toLambdaTerm |> (=) initial
            | Failure _ -> false
