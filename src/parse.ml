open Ast

let parseExpression (s : string) : expression = match s with _ -> IntLiteral 0
let parseStatement (s : string) : statement = match s with _ -> Continue
