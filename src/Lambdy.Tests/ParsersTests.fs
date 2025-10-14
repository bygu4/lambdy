namespace Lambdy.Tests

open NUnit.Framework
open FsUnit
open FParsec

open Lambdy.Lib.ParseTree
open Lambdy.Lib.Parsers

module ParsersTests =

    // ============= Constructors =============================================

    let vars = List.map Name >> Names
    let abs names term = Abstraction (vars names, term)

    let app1 operand = Application (operand, Epsilon)

    let app2 operand1 operand2 =
        Application (operand1, WithContinuation (operand2, Epsilon))

    let app3 operand1 operand2 operand3 =
        Application (operand1, WithContinuation (operand2, WithContinuation (operand3, Epsilon)))

    let closingAbs2 operand1 names term =
        Application (operand1, ClosingAbstraction (abs names term))

    let closingAbs3 operand1 operand2 names term =
        Application (operand1, WithContinuation (operand2, ClosingAbstraction (abs names term)))

    let varOp = Name >> Variable
    let varTerm = varOp >> app1

    let brAbs names = abs names >> Brackets
    let brAbsTerm names = brAbs names >> app1

    let brApp1 = app1 >> Brackets
    let brApp2 operand1 = app2 operand1 >> Brackets
    let brApp3 operand1 operand2 = app3 operand1 operand2 >> Brackets

    let brVar = varOp >> brApp1
    let brVarTerm = brVar >> app1

    // ============= Test callbacks ===========================================

    let runTest (parser : Parser<'a, unit>) (testCases : (string * 'a option) seq) =
        for input, expectedResult in testCases do
            let result = run parser input

            match result, expectedResult with
            | Success (result, _, _), Some expectedResult -> result |> should equal expectedResult
            | Failure _, None -> ()
            | _ -> failwith "Results don't match!"

    [<Test>]
    let testParser_Whitespace () =
        [
            "", None
            " ", Some ()
            "\n", Some ()
            "  \t\t    a", Some ()
            "a1", None
        ]
        |> runTest whitespace

    [<Test>]
    let testParser_WhitespaceOpt () =
        [
            "", Some ()
            "\t", Some ()
            "  \t\t  ", Some ()
            "   \n", Some ()
            "afafa", Some ()
        ]
        |> runTest whitespaceOpt

    [<Test>]
    let testParser_Comment () =
        [
            "# this is a comment", Some ()
            "#", Some ()
            "#      ", Some ()
            "    #", None
            "   \n", None
            "a b c", None
            "  1   ", None
        ]
        |> runTest inputEnd

    [<Test>]
    let testParser_Variable () =
        [
            "x", Some <| Name "x"
            "ab", Some <| Name "ab"
            "RE0", Some <| Name "RE0"
            "ab_123", Some <| Name "ab_123"
            "p33_", Some <| Name "p33_"
            "_nm", None
            "100qwe", None
            "ololo%", Some <| Name "ololo"
            "  ,  ", None
            "@test", None
            "let", None
            "letvar", Some <| Name "letvar"
            "not_a_let", Some <| Name "not_a_let"
            "exit  ", Some <| Name "exit"
            "scope", Some <| Name "scope"
            "x''", Some <| Name "x''"
            "qwert'''_yuiop", Some <| Name "qwert'''_yuiop"
            "   a", None
        ]
        |> runTest variable

    [<Test>]
    let testParser_Variables () =
        [
            "", None
            "a", Some <| vars [ "a" ]
            "    X Y Z", None
            "x1 x2 x3 x4", Some <| vars [ "x1" ; "x2" ; "x3" ; "x4" ]
            "v_1     v_2 v_3", Some <| vars [ "v_1" ; "v_2" ; "v_3" ]
            "a _ b", Some <| vars [ "a" ]
            "1 2", None
            "ololo ololo  ", Some <| vars [ "ololo" ; "ololo" ]
            "first, second", Some <| vars [ "first" ]
            "fst   snd thd   \t\n", Some <| vars [ "fst" ; "snd" ; "thd" ]
            "% VAR ^", None
            "x'  x''  x'''", Some <| vars [ "x'" ; "x''" ; "x'''" ]
        ]
        |> runTest variables

    [<Test>]
    let testParser_Operand () =
        [
            "var1", Some <| varOp "var1"
            "()", None
            "(some_var)", Some <| brVar "some_var"
            "(   XYZ )", Some <| brVar "XYZ"
            "_not_a_var", None
            "A B", Some <| varOp "A"
            "((var)", None
            "( ((ololo ))  )", Some <| brApp1 (brApp1 (brVar "ololo"))
            "\\x.y", None
            "(first  second )", Some <| brApp2 (varOp "first") (varOp "second")
            "(\\u v w.v)", Some <| brAbs [ "u" ; "v" ; "w" ] (varTerm "v")
            "((\\x.x) i j)", Some <| brApp3 (brAbs [ "x" ] (varTerm "x")) (varOp "i") (varOp "j")
        ]
        |> runTest operand

    [<Test>]
    let testParser_Application () =
        [
            "a", Some <| app1 (varOp "a")
            "(var_1)", Some <| brVarTerm "var_1"
            "X Y", Some <| app2 (varOp "X") (varOp "Y")
            "( ololo ) (abc)", Some <| app2 (brVar "ololo") (brVar "abc")
            "a1    a2    a3", Some <| app3 (varOp "a1") (varOp "a2") (varOp "a3")
            "(_xyz)", None
            "(var1) 2var", Some <| brVarTerm "var1"
            "(a b)   c", Some <| app2 (brApp2 (varOp "a") (varOp "b")) (varOp "c")
            "var (\\x.x z)", Some <| app2 (varOp "var") (brAbs [ "x" ] (app2 (varOp "x") (varOp "z")))
            "var1  (var2)   var3  \t ", Some <| app3 (varOp "var1") (brVar "var2") (varOp "var3")
            "( \\x.x )", Some <| brAbsTerm [ "x" ] (varTerm "x")
            "left \\x.right ", Some <| closingAbs2 (varOp "left") [ "x" ] (varTerm "right")
            "I J \\x.K", Some <| closingAbs3 (varOp "I") (varOp "J") [ "x" ] (varTerm "K")
            "(  (\\A1 B2 C3.A1) ololo)  var1",
            Some
            <| app2 (brApp2 (brAbs [ "A1" ; "B2" ; "C3" ] (varTerm "A1")) (varOp "ololo")) (varOp "var1")
            "( O  \\K.K A) S",
            Some
            <| app2 (Brackets (closingAbs2 (varOp "O") [ "K" ] (app2 (varOp "K") (varOp "A")))) (varOp "S")
        ]
        |> runTest application

    [<Test>]
    let testParser_Abstraction () =
        [
            "abc.d", None
            "\\FST SND", None
            "\\x.  ", None
            "λ  . U V W", None
            "\\ . ", None
            "\\x.x", Some <| abs [ "x" ] (varTerm "x")
            "λa b c.d", Some <| abs [ "a" ; "b" ; "c" ] (varTerm "d")
            "\\var1    var2.(var3)", Some <| abs [ "var1" ; "var2" ] (brVarTerm "var3")
            "\\_a.b", None
            "\\x  y  z  (k)", None
            "λx.λy.\\z.x", Some <| abs [ "x" ] (abs [ "y" ] (abs [ "z" ] (varTerm "x")))
            "λx y z.x y z", Some <| abs [ "x" ; "y" ; "z" ] (app3 (varOp "x") (varOp "y") (varOp "z"))
            "\\   u v  .  \\x. x v", Some <| abs [ "u" ; "v" ] (abs [ "x" ] (app2 (varOp "x") (varOp "v")))
            "\\ A   B C. \t B (C A)",
            Some
            <| abs [ "A" ; "B" ; "C" ] (app2 (varOp "B") (brApp2 (varOp "C") (varOp "A")))
            "λfirst second.λvar.first second",
            Some
            <| abs [ "first" ; "second" ] (abs [ "var" ] (app2 (varOp "first") (varOp "second")))
        ]
        |> runTest abstraction

    [<Test>]
    let testParser_Term () =
        [
            "_q", None
            "\\x.\\y", None
            "A B (x.y)", None
            "var1", Some <| varTerm "var1"
            "((ololo) )", Some <| app1 (brApp1 (brVar "ololo"))
            "(\\x y.x)", Some <| brAbsTerm [ "x" ; "y" ] (varTerm "x")
            "(term))", Some <| brVarTerm "term"
            "a, b, c", Some <| varTerm "a"
            "a b (\\x.x)", Some <| app3 (varOp "a") (varOp "b") (brAbs [ "x" ] (varTerm "x"))
            "S K   K ", Some <| app3 (varOp "S") (varOp "K") (varOp "K")
            "(  \\ a b  . z)", Some <| brAbsTerm [ "a" ; "b" ] (varTerm "z")
            "\\U. U (\\x.x) V", Some <| abs [ "U" ] (app3 (varOp "U") (brAbs [ "x" ] (varTerm "x")) (varOp "V"))
            "\\X Y  Z.Z \\t.X Y",
            Some
            <| abs [ "X" ; "Y" ; "Z" ] (closingAbs2 (varOp "Z") [ "t" ] (app2 (varOp "X") (varOp "Y")))
            "\\x y z.x z (y z)",
            Some
            <| abs [ "x" ; "y" ; "z" ] (app3 (varOp "x") (varOp "z") (brApp2 (varOp "y") (varOp "z")))
            "((\\a.a)   tmp)  (\\x.\\y.\\z.y z x)",
            Some
            <| app2
                (brApp2 (brAbs [ "a" ] (varTerm "a")) (varOp "tmp"))
                (brAbs [ "x" ] (abs [ "y" ] (abs [ "z" ] (app3 (varOp "y") (varOp "z") (varOp "x")))))
        ]
        |> runTest term

    [<Test>]
    let testParser_Declaration () =
        [
            "let VAR", Some <| Name "VAR"
            "let", None
            "olet a", None
            "   let xy", None
            "let    tmp", Some <| Name "tmp"
            "foo", None
            "bar ololo", None
            "let let", None
            "let help", Some <| Name "help"
            "let exitCode", Some <| Name "exitCode"
        ]
        |> runTest declaration

    [<Test>]
    let testParser_Binding () =
        [
            "let a = b", Some <| Binding (Name "a", varTerm "b")
            "let   tmp = x y z", Some <| Binding (Name "tmp", app3 (varOp "x") (varOp "y") (varOp "z"))
            "let ololo", None
            "  let var1 = var2", None
            "let A B", None
            "let x != y", None
            "foo bar = baz", None
            "let v1    =   v2   ", Some <| Binding (Name "v1", varTerm "v2")
            "let term = ", None
            "let let = X Y Z", None
            "let clear = I x x", Some <| Binding (Name "clear", app3 (varOp "I") (varOp "x") (varOp "x"))
            "let test = (\\x.x y)", Some <| Binding (Name "test", brAbsTerm [ "x" ] (app2 (varOp "x") (varOp "y")))
        ]
        |> runTest binding

    [<Test>]
    let testParser_Command () =
        [
            "exit", None
            "!   help", None
            "!reset", Some <| Command Reset
            "!help", Some <| Command Help
            "!clear   ", Some <| Command Clear
            "!exit", Some <| Command Exit
            "not_help", None
            "!exitCode", Some <| Command Exit
            "   clear", None
            "!exit  \t  ", Some <| Command Exit
            "!reset vars", Some <| Command Reset
            "!trololo", None
            "exxxit", None
        ]
        |> runTest command

    [<Test>]
    let testParser_Expression_Interactive () =
        [
            "\n", Some Empty
            "       \n", Some Empty
            " # this is a comment", Some Empty
            "  let I = \\x.x", None
            "_wewq", None
            "123\n", None
            "a clear", Some <| Result (app2 (varOp "a") (varOp "clear"))
            "\n  exit", None
            "", Some Empty
            "variable", Some <| Result (varTerm "variable")
            "let test = (\\x.x) y,  ", None
            "!help  \t#", Some <| Command Help
            "!clear buff", None
            "let K  = \\x y.x", Some <| Binding (Name "K", abs [ "x" ; "y" ] (varTerm "x"))
            "S K K   # some text \t\n", Some <| Result (app3 (varOp "S") (varOp "K") (varOp "K"))
            "let S = \\x y z.x z (y z)\n",
            Some
            <| Binding (
                Name "S",
                abs [ "x" ; "y" ; "z" ] (app3 (varOp "x") (varOp "z") (brApp2 (varOp "y") (varOp "z")))
            )
            "let ololo_exit = (\\x.\\y.x ) z\n",
            Some
            <| Binding (Name "ololo_exit", app2 (brAbs [ "x" ] (abs [ "y" ] (varTerm "x"))) (varOp "z"))
            "let var  = \\x.x \\y.y",
            Some
            <| Binding (Name "var", abs [ "x" ] (closingAbs2 (varOp "x") [ "y" ] (varTerm "y")))
            "\\A B.A \\x.B  \t",
            Some
            <| Result (abs [ "A" ; "B" ] (closingAbs2 (varOp "A") [ "x" ] (varTerm "B")))
            "let  tmp = (\\x  y z.some ( \\k.y z) x ) var",
            Some
            <| Binding (
                Name "tmp",
                app2
                    (brAbs
                        [ "x" ; "y" ; "z" ]
                        (app3 (varOp "some") (brAbs [ "k" ] (app2 (varOp "y") (varOp "z"))) (varOp "x")))
                    (varOp "var")
            )
        ]
        |> runTest (expression true)

    [<Test>]
    let testParser_Expression_Script () =
        [
            "\n", Some Empty
            "help", Some <| Result (varTerm "help")
            "!clear", None
            "!exit", None
            "!trololo", None
            "       \n", Some Empty
            " # this is a comment", Some Empty
            "  let I = \\x.x", None
            "_wewq", None
            "123\n", None
            "a clear", Some <| Result (app2 (varOp "a") (varOp "clear"))
            "\n  exit", None
            "", Some Empty
            "variable", Some <| Result (varTerm "variable")
            "let test = (\\x.x) y,  ", None
            "!help  \t#", None
            "!clear buff", None
            "let K  = \\x y.x", Some <| Binding (Name "K", abs [ "x" ; "y" ] (varTerm "x"))
            "S K K   # some text \t\n", Some <| Result (app3 (varOp "S") (varOp "K") (varOp "K"))
            "let S = \\x y z.x z (y z)\n",
            Some
            <| Binding (
                Name "S",
                abs [ "x" ; "y" ; "z" ] (app3 (varOp "x") (varOp "z") (brApp2 (varOp "y") (varOp "z")))
            )
            "let ololo_exit = (\\x.\\y.x ) z\n",
            Some
            <| Binding (Name "ololo_exit", app2 (brAbs [ "x" ] (abs [ "y" ] (varTerm "x"))) (varOp "z"))
            "let var  = \\x.x \\y.y",
            Some
            <| Binding (Name "var", abs [ "x" ] (closingAbs2 (varOp "x") [ "y" ] (varTerm "y")))
        ]
        |> runTest (expression false)
