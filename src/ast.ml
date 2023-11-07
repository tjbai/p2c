type ident = string (* variable identifier, just a string *)
type primitive = Int | Float | Boolean | Unknown (* primitive types *)
type unaryOp = Not

type binaryOp =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Equal
  | NotEqual
  | Lt
  | Lte
  | Gt
  | Gte

type expression =
  | BinaryOp of { operator : binaryOp; left : expression; right : expression }
  | UnaryOp of { operator : unaryOp; operand : expression }
  | FunctionCall of { name : ident; arguments : expression list }
  | Assignment of { name : ident; value : expression }

type statement =
  | Expression of expression
  | Function of {
      name : string;
      arguments : (ident * primitive) list;
      return : expression * primitive;
      body : statement list;
    }
  | For of {
      value : ident;
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

let showAst (tree : ast) : string = match tree with _ -> ""
