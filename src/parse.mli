open Ast
open Lex

val literal : string -> expression
val find_closure : token list -> l:token -> r:token -> token list * token list
val parse_expression : token list -> expression * token list
val parse_statement : token list -> statement * token list
val parse : token list -> ast * token list
val to_raw_ast : string -> ast
val to_ast : string -> ast
val map_ast_expressions : ast -> f:(expression -> expression) -> ast
val infer_type : expression -> expression
val replace_neg : expression -> expression
