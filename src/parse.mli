open Ast
open Lex

val convert : string -> expression
val parse_expression : token list -> expression * token list
val parse_statement : token list -> statement * token list
val parse : token list -> ast
val infer_types : ast -> ast
