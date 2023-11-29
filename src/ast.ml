open Sexplib.Std (* need for string_of_sexp *)

type primitive = Int | String | Boolean | Void | Unknown [@@deriving sexp]
type identifier = string [@@deriving sexp]
type coreIdentifier = Print | Input | Range [@@deriving sexp]

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
[@@deriving sexp]

type unaryOp = Not [@@deriving sexp]

type expression =
  | IntLiteral of int
  | StringLiteral of string
  | BooleanLiteral of bool
  | Identifier of identifier
  | Assignment of { name : identifier; t : primitive; value : expression }
  | BinaryOp of { operator : binaryOp; left : expression; right : expression }
  | UnaryOp of { operator : unaryOp; operand : expression }
  | FunctionCall of { name : identifier; arguments : expression list }
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

let showExpression (e : expression) : string =
  e |> sexp_of_expression |> Sexplib.Sexp.to_string

let showAst (tree : ast) : string = match tree with _ -> ""
