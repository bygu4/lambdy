module Parser.Tests

open NUnit.Framework
open FsUnit
open FParsec

open LambdaInterpreter
open Parser

let runTest (testCases: (string * Parser<'a, unit> * int option) seq) =
    for input, parser, expectedPos in testCases do
        let result = input |> run parser
        match result with
        | Success (_, _, pos) -> pos.Index |> should equal expectedPos.Value
        | _ -> expectedPos |> _.IsNone |> should be True

[<Test>]
let testParser_WhiteSpace () =
    [
        "", whitespaceOpt, Some 0;
        "\t", whitespaceOpt, Some 1;
        "  \t\t  ", whitespaceOpt, Some 6;
        "   \n", whitespaceOpt, Some 4;
        "", whitespace, None;
        " ", whitespace, Some 1;
        "\n", whitespace, Some 1;
        "  \t\t    a", whitespace, Some 8;
        "a1", whitespace, None;
    ] |> runTest

[<Test>]
let testParser_InputEnd () =
    [
        "", inputEnd, Some 0;
        "     ", inputEnd, Some 5;
        "\n", inputEnd, Some 1;
        "   \n", inputEnd, Some 4;
        "a12", inputEnd, None;
        "  1   ", inputEnd, None;
    ] |> runTest

[<Test>]
let testParser_Variable () =
    [
        "x", variable, Some 1;
        "ab", variable, Some 2;
        "RE0", variable, Some 3;
        "ab_123", variable, Some 6;
        "p33_", variable, Some 4;
        "_nm", variable, None;
        "100qwe", variable, None;
        "ololo%", variable, Some 5;
        "  ,  ", variable, None;
        "@test", variable, None;
        "let", variable, None;
        "letvar", variable, Some 6;
        "not_a_let", variable, Some 9;
        "exit", variable, None;
    ] |> runTest

[<Test>]
let testParser_Variables () =
    [
        "", variables, None;
        "a", variables, Some 1;
        "X Y Z", variables, Some 5;
        "x1 x2 x3 x4", variables, Some 11;
        "v_1     v_2 x_3", variables, Some 15;
        "a _ b", variables, None;
        "1 2", variables, None;
        "  ololo ololo  ", variables, None;
        "first, second", variables, Some 5;
    ] |> runTest

[<Test>]
let testParser_Operand () =
    [
        "var1", operand, Some 4;
        "()", operand, None;
        "(some_var)", operand, Some 10;
        "(   XYZ )", operand, Some 9;
        "_not_a_var", operand, None;
        "A B", operand, Some 1;
        "((var)", operand, None;
        "( ((ololo ))  )", operand, Some 15;
        "\\x.y", operand, None;
        "(first  second )", operand, Some 16;
        "(\\u v w.v)", operand, Some 10;
        "((\\x.x) i j)", operand, Some 12;
    ] |> runTest

[<Test>]
let testParser_Application () =
    [
        "a", application, Some 1;
        "(var_1)", application, Some 7;
        "X Y", application, Some 3;
        "( ololo ) (abc)", application, Some 15;
        "a1    a2    a3", application, Some 14;
        "(_xyz)", application, None;
        "(var1) 2var", application, Some 6;
        "(a b)   c", application, Some 9;
        "var (\\x.x z)", application, Some 12;
        "(  (\\A1 B2 C3.A1) ololo)  var1", application, Some 30;
        "var1  (var2)   var3  \t ", application, Some 19;
    ] |> runTest

[<Test>]
let testParser_Abstraction () =
    [
        "\\x.x", abstraction, Some 4;
        "\\a b c.d", abstraction, Some 8;
        "\\var1    var2.(var3)", abstraction, Some 20;
        "\\_a.b", abstraction, None;
        "abc.d", abstraction, None;
        "\\x  y  z  (k)", abstraction, None;
        "\\x.\\y.\\z.x", abstraction, Some 10;
        "\\first second.\\var.first second", abstraction, Some 31;
        "\\x y z.x y z", abstraction, Some 12;
    ] |> runTest

[<Test>]
let testParser_Term () =
    [
        "var1", term, Some 4;
        "((ololo) )", term, Some 10;
        "(\\x y.x)", term, Some 8;
        "\\x y z.x z (y z)", term, Some 16;
        "S K   K ", term, Some 7;
        "((\\a.a)   tmp)  (\\x.\\y.\\z.y z x)", term, Some 32;
        "(term))", term, Some 6;
        "\\x.\\y", term, None;
        "_q", term, None;
        "a, b, c", term, Some 1;
        "\\X Y  Z.Z \\t.X Y", term, Some 9;
    ] |> runTest

[<Test>]
let testParser_Declaration () =
    [
        "let VAR", declaration, Some 7;
        "let", declaration, None;
        "olet a", declaration, None;
        "   let xy", declaration, None;
        "let    tmp", declaration, Some 10;
        "foo", declaration, None;
        "bar ololo", declaration, None;
        "let let", declaration, None;
        "let help", declaration, None;
        "let exitCode", declaration, Some 12;
    ] |> runTest

[<Test>]
let testParser_Definition () =
    [
        "let a = b", definition, Some 9;
        "let   tmp = x y z", definition, Some 17;
        "let ololo", definition, None;
        "  let var1 = var2", definition, None;
        "let A B", definition, None;
        "let x != y", definition, None;
        "foo bar = baz", definition, None;
        "let v1    =   v2", definition, Some 16;
        "let term = ", definition, None;
        "let let = X Y Z", definition, None;
        "let clear = I x x", definition, None;
    ] |> runTest

[<Test>]
let testParser_Command () =
    [
        "reset", command, Some 5;
        "help", command, Some 4;
        "clear", command, Some 5;
        "exit", command, Some 4;
        "not_help", command, None;
        "exitCode", command, Some 4;
        "   clear", command, None;
        "exit  \t  ", command, Some 4;
        "reset vars", command, Some 5;
    ] |> runTest

[<Test>]
let testParser_Expression () =
    [
        "let S = \\x y z.x z (y z)\n", expression, Some 25;
        "let K  = \\x y.x", expression, Some 15;
        "S K K \t\n", expression, Some 8;
        "let  tmp = (\\x  y z.some ( \\k.y z) x ) var", expression, Some 42;
        "\n", expression, Some 1;
        "       \n", expression, Some 8;
        "  let I = \\x.x", expression, None;
        "_wewq", expression, None;
        "123\n", expression, None;
        "", expression, Some 0;
        "variable", expression, Some 8;
        "let test = (\\x.x) y,  ", expression, None;
        "help  \t", expression, Some 7;
        "clear buff", expression, None;
        "let ololo_exit = (\\x.\\y.x ) z\n", expression, Some 30;
        "a clear", expression, None;
        "\n  exit", expression, None;
        "let var  = \\x.x \\y.y", expression, None;
    ] |> runTest
