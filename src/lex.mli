type token =
  | Value of string
  | Bop of string
  | Uop of string
  | IntDef
  | StringDef
  | BoolDef
  | Lparen
  | Rparen
  | Comma
  | Colon
  | Indent
  | Dedent
  | Newline
  | Arrow

val strip_indent : string -> string * int
val split_and_process : string -> string list
val tokenize : string list -> token list
