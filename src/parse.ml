open Ast
open Core
open Lex

type ('a, 'b) union = A of 'a | B of 'b
type e_context = expression * token list
type s_context = statement * token list

(* Value s -> IntLiteral | StringLiteral | BooleanLiteral | Identifier *)
let literal (s : string) : expression =
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

(* Value s -> coreIdentifier | string *)
let map_fn (s : string) : (coreIdentifier, string) union =
  match s with
  | "print" -> A Print
  | "input" -> A Input
  | "range" -> A Range
  | _ -> B s

(* Operator precedence *)
let prec (op : binaryOp) : int =
  match op with
  | Equal | NotEqual -> 0
  | Lt | Lte | Gt | Gte | And | Or -> 1
  | Add | Subtract -> 2
  | Multiply | Divide -> 3

(* Look for a closing Rparen *)
let find_closure (ts : token list) : token list * token list =
  let rec aux acc ts (need : int) =
    match ts with
    | [] -> failwith "could not find closure..."
    | Rparen :: tl when need = 1 -> (List.rev acc, tl)
    | Rparen :: tl -> aux (Rparen :: acc) tl (need - 1)
    | Lparen :: tl -> aux (Lparen :: acc) tl (need + 1)
    | hd :: tl -> aux (hd :: acc) tl need
  in
  aux [] ts 1

(* Split a list of tokens on a given delimiter *)
let split_on (t : token) (ts : token list) : token list list =
  let rec aux (ts : token list) (cur : token list) (all : token list list) =
    match (ts, cur) with
    | [], [] -> all |> List.rev
    | [], _ -> cur :: all |> List.rev
    | hd :: tl, [] when equal_token hd t -> aux tl [] all
    | hd :: tl, _ when equal_token hd t -> aux tl [] (List.rev cur :: all)
    | hd :: tl, _ -> aux tl (hd :: cur) all
  in
  aux ts [] []

let rec parse_fn_call (fn : string) (tl : token list) : e_context =
  let rec parse_arguments tl (args : expression list) : expression list =
    match tl with
    | [] -> List.rev args
    | Comma :: tl -> parse_arguments tl args
    | _ ->
        let arg, tl = parse_expression tl in
        parse_arguments tl (arg :: args)
  in

  let args, tl = find_closure tl in
  let arguments = parse_arguments args [] in
  ( (match map_fn fn with
    | A name -> CoreFunctionCall { name; arguments }
    | B name -> FunctionCall { name; arguments }),
    tl )

(* This version of parse_expression implements the
    shunting-yard algorithm to properly handle
   operator precedence *)
(* NOTE: Refactor for readability *)
and parse_expression (ts : token list) : e_context =
  let rec aux ts (es : expression list) (ops : binaryOp list) =
    match ts with
    (* function *)
    | Value fn :: Lparen :: tl ->
        let fn_call, tl = parse_fn_call fn tl in
        aux tl (fn_call :: es) ops
    (* parentheses *)
    | Lparen :: tl ->
        let inside, tl = find_closure tl in
        let e, _ = parse_expression inside in
        aux tl (e :: es) ops
    (* unary op *)
    | Uop op :: tl ->
        let operator = map_uop op in
        let operand, tl = parse_expression tl in
        aux tl (UnaryOp { operator; operand } :: es) ops
    (* value *)
    | Value s :: tl -> aux tl (literal s :: es) ops
    (* binary op *)
    | Bop op :: tl -> (
        let cop = map_bop op in
        match (ops, es) with
        | top :: ops, right :: left :: es when prec top >= prec cop ->
            aux ts (BinaryOp { operator = top; left; right } :: es) ops
        | _ -> aux tl es (cop :: ops))
    (* While ops still exist, pop *)
    | _ -> (
        match (ops, es) with
        | topop :: remops, right :: left :: remes ->
            aux ts (BinaryOp { operator = topop; left; right } :: remes) remops
        (* Base case *)
        | _, [ x ] -> (x, ts)
        | _ -> failwith "something went wrong")
  in

  (* Check for assignment *)
  match ts with
  | Value name :: Assign :: tl ->
      let value, tl = parse_expression tl in
      (Assignment { name; t = Unknown; value }, tl)
  | _ -> aux ts [] []

(* Parse a single statement *)
let rec parse_statement (ts : token list) : s_context =
  match ts with
  | FunDef :: tl -> (Break, tl)
  | For :: tl -> (Break, tl)
  | While :: tl -> (Break, tl)
  | (If | Elif | Else) :: tl -> (Break, tl)
  | _ -> failwith "incomplete"

(* Parse a list of statements *)
and parse (ts : token list) : ast =
  let rec aux (tl : token list) (acc : ast) : ast * token list =
    match tl with
    | Newline :: tl -> aux tl acc
    | [] -> (List.rev acc, [])
    | _ ->
        let new_statement, tl = parse_statement tl in
        aux tl (new_statement :: acc)
  in
  match aux ts [] with ast, _ -> ast

(* DFS to infer assignment types from leaf literals *)
let infer_types (ast : ast) : ast = ast

(* DEPRECATED STUFF *)

(* Naive parse implementation that
    doesn't consider operator precedence *)
let rec _parse_expression (ts : token list) : e_context =
  match ts with
  (* name = tl *)
  | Value name :: Assign :: tl ->
      let value, tl = _parse_expression tl in
      (Assignment { name; t = Unknown; value }, tl)
  (* op tl *)
  | Uop op :: tl ->
      let operator = map_uop op in
      let operand, tl = _parse_expression tl in
      (UnaryOp { operator; operand }, tl)
  (* s op tl *)
  | Value s :: Bop op :: tl ->
      let operator = map_bop op in
      let left = literal s in
      let right, tl = _parse_expression tl in
      (BinaryOp { operator; left; right }, tl)
  (* fn(expression) tl *)
  | Value fn :: Lparen :: tl -> (
      let fn_call, tl = parse_fn_call fn tl in
      match tl with
      | Bop op :: tl' ->
          let operator = map_bop op in
          let right, tl' = _parse_expression tl' in
          (BinaryOp { operator; left = fn_call; right }, tl')
      | _ -> (fn_call, tl))
  (* (expression) tl *)
  | Lparen :: tl -> (
      let closure, tl = find_closure tl in
      let left, _ = _parse_expression closure in
      match tl with
      (* (expression) bop tl' *)
      | Bop op :: tl' ->
          let operator = map_bop op in
          let right, tl' = _parse_expression tl' in
          (BinaryOp { operator; left; right }, tl')
      (* (expression) tl *)
      | _ -> (left, tl))
  (* base case *)
  | Value s :: tl -> (literal s, tl)
  | [] -> failwith "tried to parse empty expression"
  | _ -> failwith "malformed %s"
