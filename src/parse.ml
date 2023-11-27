open Ast
open Core
open Lex

(* NOTE: Nead to figure how to thread context along through different calls *)

(* s -> IntLiteral | StringLiteral | BooleanLiteral | Identifier *)
let convert (s : string) : expression =
  match (s, int_of_string_opt s) with
  | "False", _ -> BooleanLiteral false
  | "True", _ -> BooleanLiteral true
  | _ when Char.(s.[0] = '\"' || s.[0] = '\'') ->
      StringLiteral (String.sub s ~pos:1 ~len:(String.length s - 2))
  | _, Some n -> IntLiteral n
  | _, None -> Identifier s

(* Uop s -> Not | ... *)
let map_uop (s : string) : unaryOp =
  match s with "not" -> Not | _ -> failwith "invalid unary op"

(* Bop s -> Add | Subtract | ... *)
let map_bop (s : string) : binaryOp =
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

type ('a, 'b) union = A of 'a | B of 'b

let _map_fn (s : string) : (coreIdentifier, identifier) union =
  match s with
  | "print" -> A Print
  | "input" -> A Input
  | "range" -> A Range
  | _ -> B s

(* Parse a complete expression from tokens *)
let rec parse_expression (ts : token list) : expression * token list =
  match ts with
  (* name = tl *)
  | Value name :: Assign :: tl ->
      let value, tl = parse_expression tl in
      (Assignment { name; t = Unknown; value }, tl)
  (* s op tl *)
  | Value s :: Bop op :: tl ->
      let operator = map_bop op in
      let left = convert s in
      let right, tl = parse_expression tl in
      (BinaryOp { operator; left; right }, tl)
  (* fn(expression) tl *)
  | Value _ :: Lparen :: _ -> failwith "incomplete"
  (* op tl *)
  | Uop op :: tl ->
      let operator = map_uop op in
      let operand, tl = parse_expression tl in
      (UnaryOp { operator; operand }, tl)
  (* (expression) tl *)
  | Lparen :: _ -> failwith "incomplete"
  (* base case *)
  | Value s :: tl -> (convert s, tl)
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
