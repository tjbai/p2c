open Ast
open Lex

val convert : string -> expression
val find_closure : token list -> token list * token list
val parse_expression : token list -> expression * token list
val parse_statement : token list -> statement * token list
val parse : token list -> ast
val infer_types : ast -> ast
