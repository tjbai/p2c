(* All tokens! *)
type token =
  (* Building blocks *)
  | Value of string
  | Bop of string
  | Uop of string
  (* Definitions *)
  | Assign
  | FunDef
  | Arrow
  | Return
  | IntDef
  | StringDef
  | BoolDef
  (* Control flow *)
  | For
  | In
  | While
  | If
  | Elif
  | Else
  (* Semantics *)
  | Lparen
  | Rparen
  | Comma
  | Colon
  | Indent
  | Dedent
  (* End of every line *)
  | Newline

val strip_indent : string -> string * int
val split_and_process : string -> string list
val tokenize_line : string list -> token list
val tokenize : string -> token list
