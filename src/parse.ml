open Ast
open Core
open Lex

[@@@warning "-27"]
[@@@warning "-32"]

type ('a, 'b) union = A of 'a | B of 'b
type e_context = expression * token list
type s_context = statement * token list
type a_context = ast * token list
type params = (string * primitive) list

(* For matching against invariants the type system can't catch *)
let impossible () = failwith "reaching this case should be impossible"

(* TODO: make all failwith messages more descriptive / debuggable *)

let literal (s : string) : expression =
  match (s, int_of_string_opt s) with
  | "False", _ -> BooleanLiteral false
  | "True", _ -> BooleanLiteral true
  | _ when Char.(s.[0] = '\"' || s.[0] = '\'') ->
      StringLiteral (String.sub s ~pos:1 ~len:(String.length s - 2))
  | _, Some n -> IntLiteral n
  | _, None -> Identifier s

let map_uop (s : string) : unaryOp =
  match s with "not" -> Not | _ -> failwith "invalid unary op"

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

let map_fn (s : string) : (coreIdentifier, string) union =
  match s with "print" -> A Print | "input" -> A Input | _ -> B s

let map_t (t : token) : primitive =
  match t with
  | IntDef -> Int
  | StringDef -> String
  | BoolDef -> Boolean
  | _ -> Unknown

(* Operator precedence *)
let prec (op : binaryOp) : int =
  match op with
  | Equal | NotEqual -> 0
  | Lt | Lte | Gt | Gte | And | Or -> 1
  | Add | Subtract -> 2
  | Multiply | Divide -> 3

let find_closure (ts : token list) ~l ~r : token list * token list =
  let rec aux acc tl (need : int) =
    match tl with
    | hd :: tl when equal_token hd r && need = 1 -> (List.rev acc, tl)
    | hd :: tl when equal_token hd r -> aux (r :: acc) tl (need - 1)
    | hd :: tl when equal_token hd l -> aux (l :: acc) tl (need + 1)
    | hd :: tl -> aux (hd :: acc) tl need
    | [] -> failwith "no closure on malformed expression"
  in

  aux [] ts 1

let find_rparen = find_closure ~l:Lparen ~r:Rparen
let find_dedent = find_closure ~l:Indent ~r:Dedent

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

(* Parse everything after name(... *)
let rec parse_fn_call (fn : string) (tl : token list) : e_context =
  let rec parse_arguments tl (args : expression list) : expression list =
    match tl with
    | [] -> List.rev args
    | Comma :: tl -> parse_arguments tl args
    | _ ->
        let arg, tl = parse_expression tl in
        parse_arguments tl (arg :: args)
  in

  let args, tl = find_rparen tl in
  let arguments = parse_arguments args [] in
  ( (match map_fn fn with
    | A name -> CoreFunctionCall { name; arguments }
    | B name -> FunctionCall { name; arguments }),
    tl )

(* This version of parse_expression implements the
    shunting-yard algorithm to properly handle operator precedence *)
and parse_expression (ts : token list) : e_context =
  let rec aux ts (es : expression list) (ops : binaryOp list) =
    match ts with
    (* function *)
    | Value fn :: Lparen :: tl ->
        let fn_call, tl = parse_fn_call fn tl in
        aux tl (fn_call :: es) ops
    (* parentheses *)
    | Lparen :: tl ->
        let inside, tl = find_rparen tl in
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
        | [], [ x ] -> (x, ts)
        | _ -> failwith "malformed expression")
  in

  match ts with
  | Value name :: Assign :: tl ->
      let value, tl = parse_expression tl in
      (Assignment { name; t = Unknown; value }, tl)
  | _ -> aux ts [] []

(* Parse everything after `def name(` *)
let parse_fn_def (ts : token list) : params * primitive * token list =
  let rec aux tl (ps : params) (ret_t : primitive) =
    match tl with
    | Value name :: Colon :: t :: tl -> aux tl ((name, map_t t) :: ps) ret_t
    | Colon :: Newline :: Indent :: tl -> (List.rev ps, ret_t, tl)
    | Arrow :: t :: tl -> aux tl ps (map_t t)
    | (Comma | Rparen) :: tl -> aux tl ps ret_t
    | _ -> failwith "malformed function declaration"
  in
  aux ts [] Void

(* Parse a single statement *)
let rec parse_statement (ts : token list) : s_context =
  match ts with
  | FunDef :: Value name :: Lparen :: tl ->
      let parameters, return, tl = parse_fn_def tl in
      let body, tl = parse_body tl in
      (Function { name; parameters; body; return }, tl)
  | For :: Value value :: In :: Value "range" :: Lparen :: tl ->
      parse_for tl value
  | While :: tl -> (Break, tl)
  | (If | Elif | Else) :: tl -> (Break, tl)
  | Return :: tl ->
      let expression, tl = parse_expression tl in
      (Return expression, tl)
  | Lex.Break :: tl -> (Ast.Break, tl)
  | Lex.Continue :: tl -> (Ast.Continue, tl)
  | _ ->
      let expression, tl = parse_expression ts in
      (Expression expression, tl)

(* Parse a list of statements *)
and parse (ts : token list) : a_context =
  let rec aux tl (acc : ast) =
    match tl with
    | Newline :: tl -> aux tl acc
    | [] -> (List.rev acc, [])
    | _ ->
        let statement, tl = parse_statement tl in
        aux tl (statement :: acc)
  in
  aux ts []

and parse_body (ts : token list) : a_context =
  let body_ts, tl = find_dedent ts in
  match parse body_ts with body, _ -> (body, tl)

(* Parse everything after for _ in range(... *)
(* NOTE: Find more polymorphic way to parse functions
   with variable numbers of arguments *)
and parse_for (ts : token list) (value : string) : s_context =
  let fn_call, tl = parse_fn_call "range" ts in
  match tl with
  | Colon :: Newline :: Indent :: tl -> (
      let body, tl = parse_body tl in
      match fn_call with
      | CoreFunctionCall { name = _; arguments = [ upper ] } ->
          ( For
              {
                value;
                lower = IntLiteral 0;
                upper;
                increment = IntLiteral 1;
                body;
              },
            tl )
      | CoreFunctionCall { name = _; arguments = [ lower; upper ] } ->
          (For { value; lower; upper; increment = IntLiteral 1; body }, tl)
      | CoreFunctionCall { name = _; arguments = [ lower; upper; increment ] }
        ->
          (For { value; lower; upper; increment; body }, tl)
      | CoreFunctionCall _ -> failwith "malformed range call"
      | _ -> impossible ())
  | _ -> failwith "malformed for loop"

(* DFS to infer assignment types from leaf literals *)
let infer_types (ast : ast) : ast = ast
