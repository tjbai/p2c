(* All tokens! *)
type token =
  (* Expressions *)
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
  | Break
  | Continue
  (* Scoping *)
  | Lparen
  | Rparen
  | Comma
  | Colon
  | Indent
  | Dedent
  | Newline
  | Hash
[@@deriving equal, sexp]

val strip_indent : string -> string * int
val split_and_process : string -> string list
val tokenize_line : string list -> token list
val tokenize : string -> token list
val show_token : token -> string
val show_tokens : token list -> string
