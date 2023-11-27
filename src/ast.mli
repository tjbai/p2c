type primitive = Int | String | Boolean | Unknown (* primitive types *)
type identifier = string (* variable identifier, just a string *)
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
  | Identifier of identifier
  | Assignment of { name : identifier; t : primitive; value : expression }
  | BinaryOp of { operator : binaryOp; left : expression; right : expression }
  | UnaryOp of { operator : unaryOp; operand : expression }
  | FunctionCall of { name : identifier; arguments : expression list }
  | CoreFunctionCall of { name : coreIdentifier; arguments : expression list }

type statement =
  | Expression of expression
  | Function of {
      name : string;
      arguments : (identifier * primitive) list;
      returnType : primitive;
      body : statement list;
    }
  | Return of expression
  | For of {
      value : identifier;
      increment : int;
      target : int; (* exclusive upper bound *)
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
