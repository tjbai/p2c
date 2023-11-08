type identifier = string (* variable identifier, just a string *)
type primitive = Int | String | Boolean | Unknown (* primitive types *)

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
  | Identifier of identifier
  | Literal of primitive
  | Assignment of { name : identifier; value : expression }
  | BinaryOp of { operator : binaryOp; left : expression; right : expression }
  | UnaryOp of { operator : unaryOp; operand : expression }
  | FunctionCall of { name : identifier; arguments : expression list }

type statement =
  | Expression of expression
  | Function of {
      name : string;
      arguments : (identifier * primitive) list;
      return : expression * primitive;
      body : statement list;
    }
  | For of {
      value : identifier;
      increment : int;
      target : int;
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
