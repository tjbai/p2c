open Ast
open Lex

val literal : string -> expression
val find_closure : token list -> token list * token list
val split_on : token -> token list -> token list list
val parse_expression : token list -> expression * token list
val parse_statement : token list -> statement * token list
val parse : token list -> ast
val infer_types : ast -> ast
