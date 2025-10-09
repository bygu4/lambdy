namespace Lambdy.Tests

open NUnit.Framework
open FsUnit
open FParsec

open Lambdy.Lib.ParseTree
open Lambdy.Lib.Parsers

module ParsersTests =

    let vars = List.map Name
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

    let runTest (testCases : (string * Parser<'a, unit> * 'a option) seq) =
        for input, parser, expectedRes in testCases do
            match run parser input with
            | Success (res, _, _) -> res |> should equal expectedRes.Value
            | _ -> expectedRes.IsNone |> should be True

    [<Test>]
    let testParser_WhiteSpace () =
        [
            "", whitespaceOpt, Some ()
            "\t", whitespaceOpt, Some ()
            "  \t\t  ", whitespaceOpt, Some ()
            "   \n", whitespaceOpt, Some ()
            "", whitespace, None
            " ", whitespace, Some ()
            "\n", whitespace, Some ()
            "  \t\t    a", whitespace, Some ()
            "a1", whitespace, None
        ]
        |> runTest

    [<Test>]
    let testParser_InputEnd () =
        [
            "", inputEnd, Some ()
            "     ", inputEnd, Some ()
            "\n", inputEnd, Some ()
            "   \n", inputEnd, Some ()
            "a12", inputEnd, None
            "  1   ", inputEnd, None
        ]
        |> runTest

    [<Test>]
    let testParser_Variable () =
        [
            "x", variable, Some <| Name "x"
            "ab", variable, Some <| Name "ab"
            "RE0", variable, Some <| Name "RE0"
            "ab_123", variable, Some <| Name "ab_123"
            "p33_", variable, Some <| Name "p33_"
            "_nm", variable, None
            "100qwe", variable, None
            "ololo%", variable, Some <| Name "ololo"
            "  ,  ", variable, None
            "@test", variable, None
            "let", variable, None
            "letvar", variable, Some <| Name "letvar"
            "not_a_let", variable, Some <| Name "not_a_let"
            "exit", variable, None
            "scope", variable, None
        ]
        |> runTest

    [<Test>]
    let testParser_Variables () =
        [
            "", variables, None
            "a", variables, Some <| vars [ "a" ]
            "X Y Z", variables, Some <| vars [ "X" ; "Y" ; "Z" ]
            "x1 x2 x3 x4", variables, Some <| vars [ "x1" ; "x2" ; "x3" ; "x4" ]
            "v_1     v_2 v_3", variables, Some <| vars [ "v_1" ; "v_2" ; "v_3" ]
            "a _ b", variables, Some <| vars [ "a" ]
            "1 2", variables, None
            "  ololo ololo  ", variables, Some <| vars [ "ololo" ; "ololo" ]
            "first, second", variables, Some <| vars [ "first" ]
            "fst   snd thd   \t\n", variables, Some <| vars [ "fst" ; "snd" ; "thd" ]
            "% VAR ^", variables, None
        ]
        |> runTest

    [<Test>]
    let testParser_Operand () =
        [
            "var1", operand, Some <| varOp "var1"
            "()", operand, None
            "(some_var)", operand, Some <| brVar "some_var"
            "(   XYZ )", operand, Some <| brVar "XYZ"
            "_not_a_var", operand, None
            "A B", operand, Some <| varOp "A"
            "((var)", operand, None
            "( ((ololo ))  )", operand, Some <| brApp1 (brApp1 (brVar "ololo"))
            "\\x.y", operand, None
            "(first  second )", operand, Some <| brApp2 (varOp "first") (varOp "second")
            "(\\u v w.v)", operand, Some <| brAbs [ "u" ; "v" ; "w" ] (varTerm "v")
            "((\\x.x) i j)", operand, Some <| brApp3 (brAbs [ "x" ] (varTerm "x")) (varOp "i") (varOp "j")
        ]
        |> runTest

    [<Test>]
    let testParser_Application () =
        [
            "a", application, Some <| app1 (varOp "a")
            "(var_1)", application, Some <| brVarTerm "var_1"
            "X Y", application, Some <| app2 (varOp "X") (varOp "Y")
            "( ololo ) (abc)", application, Some <| app2 (brVar "ololo") (brVar "abc")
            "a1    a2    a3", application, Some <| app3 (varOp "a1") (varOp "a2") (varOp "a3")
            "(_xyz)", application, None
            "(var1) 2var", application, Some <| brVarTerm "var1"
            "(a b)   c", application, Some <| app2 (brApp2 (varOp "a") (varOp "b")) (varOp "c")
            "var (\\x.x z)", application, Some <| app2 (varOp "var") (brAbs [ "x" ] (app2 (varOp "x") (varOp "z")))
            "var1  (var2)   var3  \t ", application, Some <| app3 (varOp "var1") (brVar "var2") (varOp "var3")
            "( \\x.x )", application, Some <| brAbsTerm [ "x" ] (varTerm "x")
            "left \\x.right ", application, Some <| closingAbs2 (varOp "left") [ "x" ] (varTerm "right")
            "I J \\x.K", application, Some <| closingAbs3 (varOp "I") (varOp "J") [ "x" ] (varTerm "K")
            "(  (\\A1 B2 C3.A1) ololo)  var1",
            application,
            Some
            <| app2 (brApp2 (brAbs [ "A1" ; "B2" ; "C3" ] (varTerm "A1")) (varOp "ololo")) (varOp "var1")
            "( O  \\K.K A) S",
            application,
            Some
            <| app2 (Brackets (closingAbs2 (varOp "O") [ "K" ] (app2 (varOp "K") (varOp "A")))) (varOp "S")
        ]
        |> runTest

    [<Test>]
    let testParser_Abstraction () =
        [
            "abc.d", abstraction, None
            "\\FST SND", abstraction, None
            "\\x.  ", abstraction, None
            "\\  . U V W", abstraction, None
            "\\ . ", abstraction, None
            "\\x.x", abstraction, Some <| abs [ "x" ] (varTerm "x")
            "\\a b c.d", abstraction, Some <| abs [ "a" ; "b" ; "c" ] (varTerm "d")
            "\\var1    var2.(var3)", abstraction, Some <| abs [ "var1" ; "var2" ] (brVarTerm "var3")
            "\\_a.b", abstraction, None
            "\\x  y  z  (k)", abstraction, None
            "\\x.\\y.\\z.x", abstraction, Some <| abs [ "x" ] (abs [ "y" ] (abs [ "z" ] (varTerm "x")))
            "\\x y z.x y z", abstraction, Some <| abs [ "x" ; "y" ; "z" ] (app3 (varOp "x") (varOp "y") (varOp "z"))
            "\\   u v  .  \\x. x v", abstraction, Some <| abs [ "u" ; "v" ] (abs [ "x" ] (app2 (varOp "x") (varOp "v")))
            "\\ A   B C. \t B (C A)",
            abstraction,
            Some
            <| abs [ "A" ; "B" ; "C" ] (app2 (varOp "B") (brApp2 (varOp "C") (varOp "A")))
            "\\first second.\\var.first second",
            abstraction,
            Some
            <| abs [ "first" ; "second" ] (abs [ "var" ] (app2 (varOp "first") (varOp "second")))
        ]
        |> runTest

    [<Test>]
    let testParser_Term () =
        [
            "_q", term, None
            "\\x.\\y", term, None
            "A B (x.y)", term, Some <| app2 (varOp "A") (varOp "B")
            "var1", term, Some <| varTerm "var1"
            "((ololo) )", term, Some <| app1 (brApp1 (brVar "ololo"))
            "(\\x y.x)", term, Some <| brAbsTerm [ "x" ; "y" ] (varTerm "x")
            "(term))", term, Some <| brVarTerm "term"
            "a, b, c", term, Some <| varTerm "a"
            "a b (\\x.x)", term, Some <| app3 (varOp "a") (varOp "b") (brAbs [ "x" ] (varTerm "x"))
            "S K   K ", term, Some <| app3 (varOp "S") (varOp "K") (varOp "K")
            "(  \\ a b  . z)", term, Some <| brAbsTerm [ "a" ; "b" ] (varTerm "z")
            "\\U. U (\\x.x) V", term, Some <| abs [ "U" ] (app3 (varOp "U") (brAbs [ "x" ] (varTerm "x")) (varOp "V"))
            "\\X Y  Z.Z \\t.X Y",
            term,
            Some
            <| abs [ "X" ; "Y" ; "Z" ] (closingAbs2 (varOp "Z") [ "t" ] (app2 (varOp "X") (varOp "Y")))
            "\\x y z.x z (y z)",
            term,
            Some
            <| abs [ "x" ; "y" ; "z" ] (app3 (varOp "x") (varOp "z") (brApp2 (varOp "y") (varOp "z")))
            "((\\a.a)   tmp)  (\\x.\\y.\\z.y z x)",
            term,
            Some
            <| app2
                (brApp2 (brAbs [ "a" ] (varTerm "a")) (varOp "tmp"))
                (brAbs [ "x" ] (abs [ "y" ] (abs [ "z" ] (app3 (varOp "y") (varOp "z") (varOp "x")))))
        ]
        |> runTest

    [<Test>]
    let testParser_Declaration () =
        [
            "let VAR", declaration, Some <| Name "VAR"
            "let", declaration, None
            "olet a", declaration, None
            "   let xy", declaration, None
            "let    tmp", declaration, Some <| Name "tmp"
            "foo", declaration, None
            "bar ololo", declaration, None
            "let let", declaration, None
            "let help", declaration, None
            "let exitCode", declaration, Some <| Name "exitCode"
        ]
        |> runTest

    [<Test>]
    let testParser_Binding () =
        [
            "let a = b", binding, Some <| Binding (Name "a", varTerm "b")
            "let   tmp = x y z", binding, Some <| Binding (Name "tmp", app3 (varOp "x") (varOp "y") (varOp "z"))
            "let ololo", binding, None
            "  let var1 = var2", binding, None
            "let A B", binding, None
            "let x != y", binding, None
            "foo bar = baz", binding, None
            "let v1    =   v2", binding, Some <| Binding (Name "v1", varTerm "v2")
            "let term = ", binding, None
            "let let = X Y Z", binding, None
            "let clear = I x x", binding, None
            "let test = (\\x.x y)",
            binding,
            Some <| Binding (Name "test", brAbsTerm [ "x" ] (app2 (varOp "x") (varOp "y")))
        ]
        |> runTest

    [<Test>]
    let testParser_Command () =
        [
            "reset", command, Some <| Command Reset
            "scope", command, Some <| Command Scope
            "help", command, Some <| Command Help
            "clear", command, Some <| Command Clear
            "exit", command, Some <| Command Exit
            "not_help", command, None
            "exitCode", command, Some <| Command Exit
            "   clear", command, None
            "exit  \t  ", command, Some <| Command Exit
            "reset vars", command, Some <| Command Reset
            "exxxit", command, None
        ]
        |> runTest

    [<Test>]
    let testParser_Expression () =
        [
            "\n", expression, Some Empty
            "       \n", expression, Some Empty
            "  let I = \\x.x", expression, None
            "_wewq", expression, None
            "123\n", expression, None
            "a clear", expression, None
            "\n  exit", expression, None
            "", expression, Some Empty
            "variable", expression, Some <| Result (varTerm "variable")
            "let test = (\\x.x) y,  ", expression, None
            "help  \t", expression, Some <| Command Help
            "clear buff", expression, None
            "let K  = \\x y.x", expression, Some <| Binding (Name "K", abs [ "x" ; "y" ] (varTerm "x"))
            "S K K \t\n", expression, Some <| Result (app3 (varOp "S") (varOp "K") (varOp "K"))
            "let S = \\x y z.x z (y z)\n",
            expression,
            Some
            <| Binding (
                Name "S",
                abs [ "x" ; "y" ; "z" ] (app3 (varOp "x") (varOp "z") (brApp2 (varOp "y") (varOp "z")))
            )
            "let ololo_exit = (\\x.\\y.x ) z\n",
            expression,
            Some
            <| Binding (Name "ololo_exit", app2 (brAbs [ "x" ] (abs [ "y" ] (varTerm "x"))) (varOp "z"))
            "let var  = \\x.x \\y.y",
            expression,
            Some
            <| Binding (Name "var", abs [ "x" ] (closingAbs2 (varOp "x") [ "y" ] (varTerm "y")))
            "\\A B.A \\x.B  \t",
            expression,
            Some
            <| Result (abs [ "A" ; "B" ] (closingAbs2 (varOp "A") [ "x" ] (varTerm "B")))
            "let  tmp = (\\x  y z.some ( \\k.y z) x ) var",
            expression,
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
        |> runTest
