namespace Lambdy.Tests

open NUnit.Framework
open FsUnit

open Lambdy.Lib.Continuation
open Lambdy.Lib.ParseTree
open Lambdy.Lib.IR
open Lambdy.Lib.LambdaTerm
open Lambdy.Lib.Strategy
open Lambdy.Lib.Interfaces.IReducer
open Lambdy.Lib.Reducer

module ReducerTests =

    type ExpectedResult =
        | ShouldSucceed of normalForm : LambdaTerm * reductionCount : int
        | ShouldFail

    [<Literal>]
    let ReductionLimit = 100

    // ========================================================================
    // ============= Helpers ==================================================
    // ========================================================================

    let irVarTerm = Name >> irVar
    let lamVarTerm = Name >> lamVar

    // ------------- Combinators --------------------------------------

    let irI name =
        irVarBound (Name "I") (irAbs (Name name) (irVarTerm name))

    let irT =
        irVarBound (Name "T") (irAbs (Name "x") (irAbs (Name "y") (irVarTerm "x")))

    let irF =
        irVarBound (Name "F") (irAbs (Name "x") (irAbs (Name "y") (irVarTerm "y")))

    let irOmega =
        irVarBound (Name "omega") (irAbs (Name "x") (irApp (irVarTerm "x") (irVarTerm "x")))

    let irY =
        let y =
            irAbs (Name "x") (irApp (irVarTerm "f") (irApp (irVarTerm "x") (irVarTerm "x")))

        irVarBound (Name "Y") (irAbs (Name "f") (irApp y y))

    let lamI name = lamAbs (Name name) (lamVarTerm name)
    let lamOmega = lamAbs (Name "x") (lamApp (lamVarTerm "x") (lamVarTerm "x"))
    let lamT = lamAbs (Name "x") (lamAbs (Name "y") (lamVarTerm "x"))
    let lamF = lamAbs (Name "x") (lamAbs (Name "y") (lamVarTerm "y"))

    let lamY =
        let y =
            lamAbs (Name "x") (lamApp (lamVarTerm "f") (lamApp (lamVarTerm "x") (lamVarTerm "x")))

        lamAbs (Name "f") (lamApp y y)

    // ------------- Seq cartesian product ----------------------------

    let cartesian first second =
        let rec cartesian' first second =
            continuation {
                if Seq.isEmpty first || Seq.isEmpty second then
                    return Seq.empty
                else

                let head = Seq.head first
                let tail = Seq.tail first
                let! rest = cartesian' tail second
                return Seq.append (Seq.map (fun x -> head, x) second) rest
            } in

        cartesian' first second id

    // ========================================================================
    // ============= Test cases ===============================================
    // ========================================================================

    // ------------- Generic, any strategy ----------------------------

    let termsToReduce_Generic =
        [
            // No redexes
            irVarTerm "x"
            irApp (irVarTerm "x") (irVarTerm "y")
            irAbs (Name "x") (irVarTerm "y")
            irOmega
            irApp (irVarTerm "test") (irAbs (Name "x") (irVarTerm "y"))
            irApp (irApp (irVarTerm "A1") (irVarTerm "A2")) (irVarTerm "A3")

            // Simple reduction
            irApp irOmega (irVarTerm "y")
            irApp (irI "x") (irVarTerm "z")
            irApp (irAbs (Name "x") (irApp (irVarTerm "x") (irVarTerm "y"))) (irVarTerm "z")
            irApp (irAbs (Name "x") (irVarTerm "y")) irOmega
            irApp (irAbs (Name "k") (irApp (irApp (irVarTerm "k") (irVarTerm "k")) (irVarTerm "k"))) (irVarTerm "s")

            // Nested redex
            irApp (irVarTerm "x") (irApp (irAbs (Name "a") irOmega) (irVarTerm "z"))
            irApp (irVarTerm "S") (irApp (irApp (irApp irT (irVarTerm "a")) (irVarTerm "b")) (irVarTerm "S"))
            irApp (irApp (irI "x") (irApp (irApp irF (irVarTerm "x")) (irI "x"))) (irVarTerm "y")
            irApp (irApp (irI "x") (irVarTerm "y")) (irApp (irI "x") (irVarTerm "y"))
            irApp (irVarTerm "i") (irApp (irVarTerm "j") (irApp (irI "x") (irVarTerm "k")))

            // Multiple variable substitution
            irApp
                (irApp (irAbs (Name "a") (irAbs (Name "b") (irApp (irVarTerm "b") (irVarTerm "a")))) (irVarTerm "x"))
                (irVarTerm "y")
            irApp
                (irApp
                    (irApp (irAbs (Name "a") (irAbs (Name "b") (irAbs (Name "c") (irVarTerm "x")))) (irVarTerm "y"))
                    (irVarTerm "y"))
                (irVarTerm "y")
            irApp
                (irApp
                    (irApp
                        (irAbs
                            (Name "U")
                            (irAbs (Name "V") (irAbs (Name "W") (irAbs (Name "H") (irAbs (Name "K") (irVarTerm "W"))))))
                        (irVarTerm "a"))
                    (irVarTerm "b"))
                (irVarTerm "c")
            irApp
                (irVarTerm "trololo")
                (irApp (irApp (irAbs (Name "x") (irAbs (Name "y") (irVarTerm "x"))) (irVarTerm "A")) (irVarTerm "B"))

            // Term substitution
            irApp (irI "IDENT") (irI "IDENT")
            irApp (irAbs (Name "b") (irApp (irVarTerm "b") (irVarTerm "K"))) (irI "x")
            irApp (irI "y") (irApp (irVarTerm "x") (irVarTerm "y"))
            irApp
                (irApp
                    (irAbs (Name "a") (irAbs (Name "b") (irApp (irVarTerm "b") (irVarTerm "a"))))
                    (irApp (irVarTerm "k") (irVarTerm "k")))
                (irI "x")
            irApp
                (irAbs (Name "x") (irApp (irAbs (Name "y") (irApp (irVarTerm "x") (irVarTerm "z"))) (irVarTerm "x")))
                (irAbs (Name "y") (irApp (irVarTerm "y") (irVarTerm "y")))
            irApp
                (irApp
                    (irAbs
                        (Name "A1")
                        (irAbs (Name "B1") (irApp (irApp (irVarTerm "B1") (irVarTerm "k")) (irVarTerm "A1"))))
                    irOmega)
                (irI "x")

            // Irreducible term
            irApp irOmega irOmega
            irApp (irApp irOmega irOmega) (irApp irOmega irOmega)
            irApp irY (irVarTerm "x")
            irApp irY irY
            irApp
                (irAbs (Name "x") (irApp (irApp (irVarTerm "x") (irVarTerm "x")) (irVarTerm "x")))
                (irAbs (Name "x") (irApp (irApp (irVarTerm "x") (irVarTerm "x")) (irVarTerm "x")))

            // Alpha conversion required
            irApp (irAbs (Name "t") (irAbs (Name "y") (irVarTerm "t"))) (irApp (irVarTerm "x") (irVarTerm "y"))
            irApp (irAbs (Name "x") (irAbs (Name "y") (irApp (irVarTerm "y") (irVarTerm "x")))) (irVarTerm "y")
            irApp (irAbs (Name "z'") (irAbs (Name "z") (irApp (irVarTerm "z") (irVarTerm "z'")))) (irVarTerm "z")
            irApp
                (irAbs (Name "y") (irApp (irAbs (Name "x") (irVarTerm "y")) (irAbs (Name "x") (irVarTerm "y"))))
                (irVarTerm "x")
            irApp
                (irAbs (Name "a") (irAbs (Name "b") (irAbs (Name "c") (irApp (irVarTerm "a") (irVarTerm "b")))))
                (irApp (irVarTerm "c") (irVarTerm "b"))
            irApp
                (irApp
                    (irApp
                        (irAbs
                            (Name "a")
                            (irAbs
                                (Name "a'")
                                (irAbs (Name "a''") (irApp (irApp (irVarTerm "a''") (irVarTerm "a'")) (irVarTerm "a")))))
                        (irVarTerm "a'"))
                    (irVarTerm "a''"))
                (irVarTerm "a'''")
            irApp
                (irAbs (Name "t") (irAbs (Name "x") (irAbs (Name "y") (irApp (irVarTerm "t") (irVarTerm "t")))))
                (irApp (irVarTerm "x") (irVarTerm "y"))

            // Bound value
            irVarBound (Name "x") (irVarTerm "value")
            irVarBound (Name "var") (irApp (irVarTerm "x") (irI "x"))
            irApp (irApp (irVarTerm "A") (irVarBound (Name "B") (irVarTerm "B1"))) (irVarTerm "C")
            irApp (irVarBound (Name "temp") (irI "x")) irOmega
            irAbs (Name "var") (irVarBound (Name "x") (irApp (irVarTerm "x") (irVarTerm "y")))
            irAbs (Name "U") (irApp (irVarTerm "V") (irVarBound (Name "W") (irVarTerm "K")))
            irApp
                (irAbs (Name "x") (irApp (irVarTerm "x") (irVarTerm "y")))
                (irVarBound (Name "K") (irApp (irVarTerm "z") (irVarTerm "z")))

            // Delayed alpha conversion
            irAbsAlpha (Name "x") (Name "x'") (irVarTerm "x")
            irAbsAlpha (Name "y") (Name "y'") (irVarBound (Name "Y") (irVarTerm "y"))
            irAbsAlpha (Name "A") (Name "C") (irApp (irVarTerm "A") (irVarTerm "B"))
            irAbs (Name "I") (irAbsAlpha (Name "J") (Name "T") (irVarTerm "K"))
            irApp (irAbsAlpha (Name "x") (Name "x'") (irVarTerm "x")) (irI "y")
            irApp
                (irApp
                    (irAbs (Name "x") (irAbsAlpha (Name "y") (Name "foo") (irApp (irVarTerm "y") (irVarTerm "x"))))
                    (irVarTerm "y"))
                (irVarTerm "t")
            irApp
                (irAbs
                    (Name "x")
                    (irAbsAlpha (Name "y") (Name "y'") (irApp (irVarTerm "x") (irVarBound (Name "Y") (irVarTerm "y")))))
                (irVarTerm "y'")
        ]

    let expectedResults_Generic =
        [
            // No redexes
            ShouldSucceed (lamVarTerm "x", 0)
            ShouldSucceed (lamApp (lamVarTerm "x") (lamVarTerm "y"), 0)
            ShouldSucceed (lamAbs (Name "x") (lamVarTerm "y"), 0)
            ShouldSucceed (lamOmega, 0)
            ShouldSucceed (lamApp (lamVarTerm "test") (lamAbs (Name "x") (lamVarTerm "y")), 0)
            ShouldSucceed (lamApp (lamApp (lamVarTerm "A1") (lamVarTerm "A2")) (lamVarTerm "A3"), 0)

            // Simple reduction
            ShouldSucceed (lamApp (lamVarTerm "y") (lamVarTerm "y"), 1)
            ShouldSucceed (lamVarTerm "z", 1)
            ShouldSucceed (lamApp (lamVarTerm "z") (lamVarTerm "y"), 1)
            ShouldSucceed (lamVarTerm "y", 1)
            ShouldSucceed (lamApp (lamApp (lamVarTerm "s") (lamVarTerm "s")) (lamVarTerm "s"), 1)

            // Nested redex
            ShouldSucceed (lamApp (lamVarTerm "x") lamOmega, 1)
            ShouldSucceed (lamApp (lamVarTerm "S") (lamApp (lamVarTerm "a") (lamVarTerm "S")), 2)
            ShouldSucceed (lamVarTerm "y", 4)
            ShouldSucceed (lamApp (lamVarTerm "y") (lamVarTerm "y"), 2)
            ShouldSucceed (lamApp (lamVarTerm "i") (lamApp (lamVarTerm "j") (lamVarTerm "k")), 1)

            // Multiple variable substitution
            ShouldSucceed (lamApp (lamVarTerm "y") (lamVarTerm "x"), 2)
            ShouldSucceed (lamVarTerm "x", 3)
            ShouldSucceed (lamAbs (Name "H") (lamAbs (Name "K") (lamVarTerm "c")), 3)
            ShouldSucceed (lamApp (lamVarTerm "trololo") (lamVarTerm "A"), 2)

            // Term substitution
            ShouldSucceed (lamI "IDENT", 1)
            ShouldSucceed (lamVarTerm "K", 2)
            ShouldSucceed (lamApp (lamVarTerm "x") (lamVarTerm "y"), 1)
            ShouldSucceed (lamApp (lamVarTerm "k") (lamVarTerm "k"), 3)
            ShouldSucceed (lamApp (lamVarTerm "z") (lamVarTerm "z"), 3)
            ShouldSucceed (lamApp (lamVarTerm "k") lamOmega, 3)

            // Irreducible term
            ShouldFail
            ShouldFail
            ShouldFail
            ShouldFail
            ShouldFail

            // Alpha conversion required
            ShouldSucceed (lamAbs (Name "y'") (lamApp (lamVarTerm "x") (lamVarTerm "y")), 1)
            ShouldSucceed (lamAbs (Name "y'") (lamApp (lamVarTerm "y'") (lamVarTerm "y")), 1)
            ShouldSucceed (lamAbs (Name "z''") (lamApp (lamVarTerm "z''") (lamVarTerm "z")), 1)
            ShouldSucceed (lamVarTerm "x", 2)
            ShouldSucceed (
                lamAbs
                    (Name "b'")
                    (lamAbs (Name "c'") (lamApp (lamApp (lamVarTerm "c") (lamVarTerm "b")) (lamVarTerm "b'"))),
                1
            )
            ShouldSucceed (lamApp (lamApp (lamVarTerm "a'''") (lamVarTerm "a''")) (lamVarTerm "a'"), 3)
            ShouldSucceed (
                lamAbs
                    (Name "x'")
                    (lamAbs
                        (Name "y'")
                        (lamApp (lamApp (lamVarTerm "x") (lamVarTerm "y")) (lamApp (lamVarTerm "x") (lamVarTerm "y")))),
                1
            )

            // Bound value
            ShouldSucceed (lamVarTerm "value", 0)
            ShouldSucceed (lamApp (lamVarTerm "x") (lamI "x"), 0)
            ShouldSucceed (lamApp (lamApp (lamVarTerm "A") (lamVarTerm "B1")) (lamVarTerm "C"), 0)
            ShouldSucceed (lamOmega, 1)
            ShouldSucceed (lamAbs (Name "var") (lamApp (lamVarTerm "x") (lamVarTerm "y")), 0)
            ShouldSucceed (lamAbs (Name "U") (lamApp (lamVarTerm "V") (lamVarTerm "K")), 0)
            ShouldSucceed (lamApp (lamApp (lamVarTerm "z") (lamVarTerm "z")) (lamVarTerm "y"), 1)

            // Delayed alpha conversion
            ShouldSucceed (lamI "x'", 0)
            ShouldSucceed (lamAbs (Name "y'") (lamVarTerm "y"), 0)
            ShouldSucceed (lamAbs (Name "C") (lamApp (lamVarTerm "C") (lamVarTerm "B")), 0)
            ShouldSucceed (lamAbs (Name "I") (lamAbs (Name "T") (lamVarTerm "K")), 0)
            ShouldSucceed (lamI "y", 1)
            ShouldSucceed (lamApp (lamVarTerm "t") (lamVarTerm "y"), 2)
            ShouldSucceed (lamAbs (Name "y''") (lamApp (lamVarTerm "y'") (lamVarTerm "y")), 1)
        ]

    let testCases_Generic =
        let allStrategies = [ normalOrder ; applicativeOrder ; callByName ; callByValue ]
        let withAndWithoutEta = [ true ; false ]

        cartesian (cartesian allStrategies withAndWithoutEta) (Seq.zip termsToReduce_Generic expectedResults_Generic)
        |> Seq.map (fun ((x, y), (z, t)) -> TestCaseData (x, y, z, t))

    // ------------- Different results for strict and lazy ------------

    let termsToReduce_StrictAndLazy =
        [
            // Reduced only lazily
            irApp irF (irApp irY (irVarTerm "z"))
            irApp (irAbs (Name "x") (irVarTerm "y")) (irApp irOmega irOmega)
            irApp (irApp (irI "x") (irAbs (Name "x") (irVarTerm "z"))) (irApp irY (irVarTerm "a"))
            irApp
                (irApp
                    (irAbs (Name "S") (irAbs (Name "R") (irApp (irVarTerm "K") (irApp (irVarTerm "S") (irVarTerm "R")))))
                    irF)
                (irApp irY (irVarTerm "p"))
            irApp (irVarTerm "www") (irApp (irApp irF (irApp irY (irVarTerm "E"))) (irVarTerm "www"))
            irApp (irAbs (Name "x") (irApp irF (irApp irOmega irOmega))) (irVarTerm "p")

            // Different reduction count
            irApp
                (irAbs (Name "x") (irApp (irApp (irVarTerm "x") (irVarTerm "x")) (irVarTerm "x")))
                (irApp (irI "x") (irVarTerm "y"))
            irApp
                (irAbs (Name "y") (irApp (irApp (irVarTerm "y") (irVarTerm "a")) (irVarTerm "y")))
                (irApp irOmega (irVarTerm "x"))
            irApp
                (irAbs
                    (Name "x")
                    (irApp (irApp (irApp (irVarTerm "x") (irVarTerm "x")) (irVarTerm "x")) (irVarTerm "x")))
                (irApp (irAbs (Name "y") (irI "I")) (irVarTerm "tmp"))
        ]

    let expectedResults_Strict =
        [
            // Reduced only lazily
            ShouldFail
            ShouldFail
            ShouldFail
            ShouldFail
            ShouldFail
            ShouldFail

            // Different reduction count
            ShouldSucceed (lamApp (lamApp (lamVarTerm "y") (lamVarTerm "y")) (lamVarTerm "y"), 2)
            ShouldSucceed (
                lamApp
                    (lamApp (lamApp (lamVarTerm "x") (lamVarTerm "x")) (lamVarTerm "a"))
                    (lamApp (lamVarTerm "x") (lamVarTerm "x")),
                2
            )
            ShouldSucceed (lamI "I", 5)
        ]

    let expectedResults_Lazy =
        [
            // Reduced only lazily
            ShouldSucceed (lamI "y", 1)
            ShouldSucceed (lamVarTerm "y", 1)
            ShouldSucceed (lamVarTerm "z", 2)
            ShouldSucceed (lamApp (lamVarTerm "K") (lamI "y"), 3)
            ShouldSucceed (lamApp (lamVarTerm "www") (lamVarTerm "www"), 2)
            ShouldSucceed (lamI "y", 2)

            // Different reduction count
            ShouldSucceed (lamApp (lamApp (lamVarTerm "y") (lamVarTerm "y")) (lamVarTerm "y"), 4)
            ShouldSucceed (
                lamApp
                    (lamApp (lamApp (lamVarTerm "x") (lamVarTerm "x")) (lamVarTerm "a"))
                    (lamApp (lamVarTerm "x") (lamVarTerm "x")),
                3
            )
            ShouldSucceed (lamI "I", 8)
        ]

    let testCases_StrictAndLazy =
        let strictStrategies = [ applicativeOrder ; callByValue ]
        let lazyStrategies = [ normalOrder ; callByName ]
        let withAndWithoutEta = [ true ; false ]

        let testCases_Strict =
            cartesian
                (cartesian strictStrategies withAndWithoutEta)
                (Seq.zip termsToReduce_StrictAndLazy expectedResults_Strict)
            |> Seq.map (fun ((x, y), (z, t)) -> TestCaseData (x, y, z, t))

        let testCases_Lazy =
            cartesian
                (cartesian lazyStrategies withAndWithoutEta)
                (List.zip termsToReduce_StrictAndLazy expectedResults_Lazy)
            |> Seq.map (fun ((x, y), (z, t)) -> TestCaseData (x, y, z, t))

        Seq.append testCases_Strict testCases_Lazy

    // ------------- Different results for strong and weak ------------

    let termsToReduce_StrongAndWeak =
        [
            // Under abstraction
            irAbs (Name "a") (irApp (irAbs (Name "b") (irApp (irVarTerm "b") (irVarTerm "c"))) (irVarTerm "x"))
            irAbs (Name "X") (irApp (irApp (irI "Y") (irVarTerm "X")) irOmega)
            irAbs (Name "s") (irAbs (Name "t") (irApp (irAbs (Name "x") (irVarTerm "y")) (irVarTerm "s")))
            irAbs (Name "x") (irApp (irApp (irI "x") (irVarTerm "y")) (irVarTerm "k"))
            irAbs (Name "q") (irApp (irI "x") (irI "x"))
            irY
            irApp (irVarTerm "Q") irY
            irAbs (Name "s") irY
            irApp (irAbs (Name "T") irY) (irVarTerm "a_")

            // Partial application
            irApp (irAbs (Name "x") (irAbs (Name "y") (irApp (irVarTerm "x") (irVarTerm "K")))) (irI "S")
            irApp
                (irAbs (Name "q") (irAbs (Name "w") (irApp (irVarTerm "q") (irApp (irVarTerm "w") (irVarTerm "q")))))
                irOmega
            irApp
                (irVarTerm "S")
                (irApp (irAbs (Name "x") (irAbs (Name "y") (irApp (irVarTerm "x") (irVarTerm "S")))) irY)
        ]

    let expectedResults_Strong =
        [
            // Under abstraction
            ShouldSucceed (lamAbs (Name "a") (lamApp (lamVarTerm "x") (lamVarTerm "c")), 1)
            ShouldSucceed (lamAbs (Name "X") (lamApp (lamVarTerm "X") lamOmega), 1)
            ShouldSucceed (lamAbs (Name "s") (lamAbs (Name "t") (lamVarTerm "y")), 1)
            ShouldSucceed (lamAbs (Name "x") (lamApp (lamVarTerm "y") (lamVarTerm "k")), 1)
            ShouldSucceed (lamAbs (Name "q") (lamI "x"), 1)
            ShouldFail
            ShouldFail
            ShouldFail
            ShouldFail

            // Partial application
            ShouldSucceed (lamAbs (Name "y") (lamVarTerm "K"), 2)
            ShouldSucceed (
                lamAbs (Name "w") (lamApp (lamApp (lamVarTerm "w") lamOmega) (lamApp (lamVarTerm "w") lamOmega)),
                2
            )
            ShouldFail
        ]

    let expectedResults_Weak =
        [
            // Under abstraction
            ShouldSucceed (
                lamAbs
                    (Name "a")
                    (lamApp (lamAbs (Name "b") (lamApp (lamVarTerm "b") (lamVarTerm "c"))) (lamVarTerm "x")),
                0
            )
            ShouldSucceed (lamAbs (Name "X") (lamApp (lamApp (lamI "Y") (lamVarTerm "X")) lamOmega), 0)
            ShouldSucceed (
                lamAbs (Name "s") (lamAbs (Name "t") (lamApp (lamAbs (Name "x") (lamVarTerm "y")) (lamVarTerm "s"))),
                0
            )
            ShouldSucceed (lamAbs (Name "x") (lamApp (lamApp (lamI "x") (lamVarTerm "y")) (lamVarTerm "k")), 0)
            ShouldSucceed (lamAbs (Name "q") (lamApp (lamI "x") (lamI "x")), 0)
            ShouldSucceed (lamY, 0)
            ShouldSucceed (lamApp (lamVarTerm "Q") lamY, 0)
            ShouldSucceed (lamAbs (Name "s") lamY, 0)
            ShouldSucceed (lamY, 1)

            // Partial application
            ShouldSucceed (lamAbs (Name "y") (lamApp (lamI "S") (lamVarTerm "K")), 1)
            ShouldSucceed (lamAbs (Name "w") (lamApp lamOmega (lamApp (lamVarTerm "w") lamOmega)), 1)
            ShouldSucceed (lamApp (lamVarTerm "S") (lamAbs (Name "y") (lamApp lamY (lamVarTerm "S"))), 1)
        ]

    let testCases_StrongAndWeak =
        let strongStrategies = [ normalOrder ; applicativeOrder ]
        let weakStrategies = [ callByName ; callByValue ]
        let withAndWithoutEta = [ true ; false ]

        let testCases_Strong =
            cartesian
                (cartesian strongStrategies withAndWithoutEta)
                (Seq.zip termsToReduce_StrongAndWeak expectedResults_Strong)
            |> Seq.map (fun ((x, y), (z, t)) -> TestCaseData (x, y, z, t))

        let testCases_Weak =
            cartesian
                (cartesian weakStrategies withAndWithoutEta)
                (Seq.zip termsToReduce_StrongAndWeak expectedResults_Weak)
            |> Seq.map (fun ((x, y), (z, t)) -> TestCaseData (x, y, z, t))

        Seq.append testCases_Strong testCases_Weak

    // ------------- Different results with and w/o eta ---------------

    let termsToReduce_EtaConversions =
        [
            // Eta conversion
            irOmega
            irAbs (Name "S") (irApp (irApp (irVarTerm "S") (irVarTerm "K")) (irVarTerm "S"))
            irAbs (Name "x") (irApp (irVarTerm "hehehe") (irVarTerm "x"))
            irAbs (Name "z") (irApp (irApp (irVarTerm "x") (irVarTerm "y")) (irVarTerm "z"))
            irApp (irVarTerm "A") (irAbs (Name "VAR") (irApp (irVarTerm "A") (irVarTerm "VAR")))
            irAbs (Name "Q") (irApp (irApp (irVarTerm "T") irOmega) (irVarTerm "Q"))
            irApp (irAbs (Name "x") (irAbs (Name "y") (irApp (irVarTerm "x") (irVarTerm "y")))) (irVarTerm "z")
            irApp (irAbs (Name "a'") (irAbs (Name "a") (irApp (irVarTerm "a'") (irVarTerm "a")))) (irVarTerm "a")
            irAbs (Name "K") (irAbs (Name "x") (irApp (irVarTerm "K") (irVarTerm "x")))
            irAbs (Name "qqq") (irApp (irVarTerm "Z") (irAbs (Name "t") (irApp (irVarTerm "P") (irVarTerm "t"))))
            irAbsAlpha (Name "x") (Name "x'") (irApp (irVarTerm "Z") (irVarTerm "x"))
        ]

    let expectedResults_WithEta =
        [
            // Eta conversion
            ShouldSucceed (lamOmega, 0)
            ShouldSucceed (lamAbs (Name "S") (lamApp (lamApp (lamVarTerm "S") (lamVarTerm "K")) (lamVarTerm "S")), 0)
            ShouldSucceed (lamVarTerm "hehehe", 0)
            ShouldSucceed (lamApp (lamVarTerm "x") (lamVarTerm "y"), 0)
            ShouldSucceed (lamApp (lamVarTerm "A") (lamVarTerm "A"), 0)
            ShouldSucceed (lamApp (lamVarTerm "T") lamOmega, 0)
            ShouldSucceed (lamVarTerm "z", 1)
            ShouldSucceed (lamVarTerm "a", 1)
            ShouldSucceed (lamI "K", 0)
            ShouldSucceed (lamAbs (Name "qqq") (lamApp (lamVarTerm "Z") (lamVarTerm "P")), 0)
            ShouldSucceed (lamVarTerm "Z", 0)
        ]

    let expectedResults_WithoutEta =
        [
            // Eta conversion
            ShouldSucceed (lamOmega, 0)
            ShouldSucceed (lamAbs (Name "S") (lamApp (lamApp (lamVarTerm "S") (lamVarTerm "K")) (lamVarTerm "S")), 0)
            ShouldSucceed (lamAbs (Name "x") (lamApp (lamVarTerm "hehehe") (lamVarTerm "x")), 0)
            ShouldSucceed (lamAbs (Name "z") (lamApp (lamApp (lamVarTerm "x") (lamVarTerm "y")) (lamVarTerm "z")), 0)
            ShouldSucceed (
                lamApp (lamVarTerm "A") (lamAbs (Name "VAR") (lamApp (lamVarTerm "A") (lamVarTerm "VAR"))),
                0
            )
            ShouldSucceed (lamAbs (Name "Q") (lamApp (lamApp (lamVarTerm "T") lamOmega) (lamVarTerm "Q")), 0)
            ShouldSucceed (lamAbs (Name "y") (lamApp (lamVarTerm "z") (lamVarTerm "y")), 1)
            ShouldSucceed (lamAbs (Name "a''") (lamApp (lamVarTerm "a") (lamVarTerm "a''")), 1)
            ShouldSucceed (lamAbs (Name "K") (lamAbs (Name "x") (lamApp (lamVarTerm "K") (lamVarTerm "x"))), 0)
            ShouldSucceed (
                lamAbs
                    (Name "qqq")
                    (lamApp (lamVarTerm "Z") (lamAbs (Name "t") (lamApp (lamVarTerm "P") (lamVarTerm "t")))),
                0
            )
            ShouldSucceed (lamAbs (Name "x'") (lamApp (lamVarTerm "Z") (lamVarTerm "x'")), 0)
        ]

    let testCases_EtaConversions =
        let allStrategies = [ normalOrder ; applicativeOrder ; callByName ; callByValue ]
        let withEta = [ true ]
        let withoutEta = [ false ]

        let testCases_WithEta =
            cartesian (cartesian allStrategies withEta) (Seq.zip termsToReduce_EtaConversions expectedResults_WithEta)
            |> Seq.map (fun ((x, y), (z, t)) -> TestCaseData (x, y, z, t))

        let testCases_WithoutEta =
            cartesian
                (cartesian allStrategies withoutEta)
                (Seq.zip termsToReduce_EtaConversions expectedResults_WithoutEta)
            |> Seq.map (fun ((x, y), (z, t)) -> TestCaseData (x, y, z, t))

        Seq.append testCases_WithEta testCases_WithoutEta

    // ------------- All test cases -----------------------------------

    let testCases =
        testCases_EtaConversions
        |> Seq.append testCases_StrongAndWeak
        |> Seq.append testCases_StrictAndLazy
        |> Seq.append testCases_Generic

    // ========================================================================
    // ============= Test callback ============================================
    // ========================================================================

    [<TestCaseSource(nameof (testCases))>]
    let testReduce strategy etaConversions term expectedResult =
        let reducer =
            new Reducer (strategy = strategy, etaConversions = etaConversions, reductionLimit = ReductionLimit)

        let result = reducer.Reduce term

        match result, expectedResult with
        | Success (term, count), ShouldSucceed (expectedTerm, expectedCount) ->
            term |> should equal expectedTerm
            count |> should equal expectedCount
        | Failure _, ShouldFail -> ()
        | _ -> failwith "Results don't match!"
