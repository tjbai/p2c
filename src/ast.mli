(* primitive types *)
type primitive = Int | String | Boolean | Void | Unknown
[@@deriving sexp, equal]

(* core functions *)
type coreIdentifier = Print | Input [@@deriving sexp]

(* binary operations *)
type binaryOp =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Mod
  | And
  | Or
  | Equal
  | NotEqual
  | Lt
  | Lte
  | Gt
  | Gte
[@@deriving sexp]

type unaryOp = Not | Neg [@@deriving sexp]

type expression =
  | IntLiteral of int
  | StringLiteral of string
  | BooleanLiteral of bool
  | Identifier of string
  | Assignment of {
      name : string;
      t : primitive;
      value : expression;
      operator : binaryOp option;
    }
  | BinaryOp of { operator : binaryOp; left : expression; right : expression }
  | UnaryOp of { operator : unaryOp; operand : expression }
  | FunctionCall of { name : string; arguments : expression list }
  | CoreFunctionCall of { name : coreIdentifier; arguments : expression list }
[@@deriving sexp]

type statement =
  | Expression of expression
  | Import of string
  | Function of {
      name : string;
      parameters : (string * primitive) list;
      return : primitive;
      body : statement list;
    }
  | Return of expression
  | For of {
      value : string;
      lower : expression;
      upper : expression; (* exclusive upper bound *)
      increment : expression;
      body : statement list;
    }
  | While of { test : expression; body : statement list }
  | If of { test : expression; body : statement list }
  | Elif of { test : expression; body : statement list }
  | Else of { body : statement list }
  | Comment of string
  | Pass
  | Break
  | Continue
[@@deriving sexp]

type ast = statement list [@@deriving sexp]

val showAst : ast -> string
val showExpression : expression -> string
