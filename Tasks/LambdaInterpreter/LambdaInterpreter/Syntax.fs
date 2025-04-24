namespace LambdaInterpreter

type Variable = Name of string

type Variables = Variable list

type Term =
    | Application of Operand * ApplicationOpt
    | Abstraction of Variables * Term
and ApplicationOpt =
    | Apply of Operand * ApplicationOpt
    | Epsilon
and Operand =
    | Variable of Variable
    | Brackets of Term

type Expression =
    | Definition of Variable * Term
    | Result of Term
    | Epsilon

type Program = Expression list
