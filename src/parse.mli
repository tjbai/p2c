open Ast

val parseExpression : string -> expression
val parseStatement : string -> statement
val getIndentLevel : string -> int
