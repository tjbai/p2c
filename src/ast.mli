type primitive = Int | String | Boolean | Unknown (* primitive types *)
type coreIdentifier = Print | Input | Range (* core functions *)

(* binary operations *)
type binaryOp =
  | Add
  | Subtract
  | Multiply
  | Divide
  | And
  | Or
  | Equal
  | NotEqual
  | Lt
  | Lte
  | Gt
  | Gte

type unaryOp = Not

type expression =
  | IntLiteral of int
  | StringLiteral of string
  | BooleanLiteral of bool
  | Identifier of string
  | Assignment of { name : string; t : primitive; value : expression }
  | BinaryOp of { operator : binaryOp; left : expression; right : expression }
  | UnaryOp of { operator : unaryOp; operand : expression }
  | FunctionCall of { name : string; arguments : expression list }
  | CoreFunctionCall of { name : coreIdentifier; arguments : expression list }
[@@deriving sexp]

type statement =
  | Expression of expression
  | Function of {
      name : string;
      parameters : (string * primitive) list;
      return : primitive;
      body : statement list;
    }
  | Return of expression
  | For of {
      value : string;
      lower : int;
      upper : int; (* exclusive upper bound *)
      increment : int;
      body : statement list;
    }
  | While of { test : expression; body : statement list }
  | If of { test : expression; body : statement list }
  | Elif of { test : expression; body : statement list }
  | Else of { body : statement list }
  | Pass
  | Break
  | Continue

type ast = statement list

val showAst : ast -> string
val showExpression : expression -> string
