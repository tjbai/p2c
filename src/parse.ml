open Ast
open Core
open Lex

(* NOTE: Nead to figure how to thread context along through different calls *)

(* Value s -> IntLiteral | StringLiteral | BooleanLiteral | Identifier *)
let convert (t : token) : expression =
  match t with
  | Value s -> (
      match s with
      | _ when Char.(s.[0] = '\"' || s.[0] = '\'') -> StringLiteral ""
      | _ -> IntLiteral 0)
  | _ -> failwith "can only convert Value type"

(* Uop s -> Not | ... *)
let match_uop (s : string) : unaryOp =
  match s with "not" -> Not | _ -> failwith "invalid unary op"

(* Bop s -> Add | Subtract | ... *)
let match_bop (s : string) : binaryOp =
  match s with
  | "+" -> Add
  | "-" -> Subtract
  | "*" -> Multiply
  | "/" -> Divide
  | "and" -> And
  | "or" -> Or
  | "==" -> Equal
  | "!=" -> NotEqual
  | "<" -> Lt
  | "<=" -> Lte
  | ">" -> Gt
  | ">=" -> Gte
  | _ -> failwith "invalid binary op"

(* Parse a complete expression from tokens *)
let rec parse_expression (ts : token list) : expression * token list =
  match ts with
  | Value name :: Assign :: tl ->
      let value, tl = parse_expression tl in
      (Assignment { name; t = Unknown; value }, tl)
  | Value _ :: _ -> failwith "incomplete"
  | Uop op :: tl ->
      let operator = match_uop op in
      let operand, tl = parse_expression tl in
      (UnaryOp { operator; operand }, tl)
  | Lparen :: _ -> failwith "incomplete"
  | [] -> failwith "tried to parse empty expression"
  | _ -> failwith "malformed %s"

(* Parse a complete statement from tokens *)
let parse_statement (ts : token list) : statement * token list =
  match ts with _ -> (Break, [])

let parse (tokens : token list) : ast =
  let rec aux (t : token list) (acc : ast) : ast * token list =
    match t with
    (* Out of tokens, return acc *)
    | [] -> (List.rev acc, [])
    (* Skip empty lines *)
    | Newline :: tl -> aux tl acc
    | _ -> (
        match parse_statement t with
        | new_statement, tl -> aux tl (new_statement :: acc))
  in
  match aux tokens [] with ast, _ -> ast

let infer_types (ast : ast) : ast = ast
