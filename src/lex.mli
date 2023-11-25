type token =
  | Identifier of string
  | Primitive of string
  | BinaryOp of string
  | Lparen
  | Rparen
  | Comma
  | Colon
  | Indent (* start of a block *)
  | Dedent (* end of a block *)
  | Newline
  | Arrow (* -> in function type annotations *)
  | EOF

val strip_indent : string -> string * int
val tokenize : string list -> token list
