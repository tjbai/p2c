open Ast
open Core
open Lex

[@@@warning "-27"]
[@@@warning "-32"]

type e_context = expression * token list
type s_context = statement * token list
type a_context = ast * token list
type params = (string * primitive) list

(* see note below about mutation *)
type type_tbl = (string, primitive) Core.Hashtbl.t

let impossible () = failwith "reaching this case should be impossible"

(***********************************************************************************************)

let literal (s : string) : expression =
  match (s, int_of_string_opt s) with
  | "False", _ -> BooleanLiteral false
  | "True", _ -> BooleanLiteral true
  | _ when Char.(s.[0] = '\"' || s.[0] = '\'') ->
      StringLiteral (String.sub s ~pos:1 ~len:(String.length s - 2))
  | _, Some n -> IntLiteral n
  | _, None -> Identifier s

let map_uop (s : string) : unaryOp =
  match s with
  | "not" -> Not
  | "-" -> Neg
  | _ -> failwith (sprintf "invalid unary op %s" s)

let map_bop (s : string) : binaryOp =
  match s with
  | "+" -> Add
  | "-" -> Subtract
  | "*" -> Multiply
  | "/" -> Divide
  | "%" -> Mod
  | "and" -> And
  | "or" -> Or
  | "==" -> Equal
  | "!=" -> NotEqual
  | "<" -> Lt
  | "<=" -> Lte
  | ">" -> Gt
  | ">=" -> Gte
  | _ -> failwith (sprintf "invalid binary op %s" s)

let map_t (t : token) : primitive =
  match t with
  | IntDef -> Int
  | StringDef -> String
  | BoolDef -> Boolean
  | _ -> Unknown

let prec (op : binaryOp) : int =
  match op with
  | Equal | NotEqual -> 0
  | Lt | Lte | Gt | Gte | And | Or -> 1
  | Add | Subtract -> 2
  | Multiply | Divide | Mod -> 3

(* Find the matching closing character for an opener *)
(* Could use fold_until, but we also need to return the tail... this way cleaner? *)
let find_closure (ts : token list) ~l ~r : token list * token list =
  let rec aux acc tl (need : int) =
    match tl with
    | hd :: tl when equal_token hd r && need = 1 -> (List.rev acc, tl)
    | hd :: tl when equal_token hd r -> aux (r :: acc) tl (need - 1)
    | hd :: tl when equal_token hd l -> aux (l :: acc) tl (need + 1)
    | hd :: tl -> aux (hd :: acc) tl need
    | [] ->
        failwith
          (sprintf "could not find closure for %s in %s" (l |> show_token)
             (ts |> show_tokens))
  in
  aux [] ts 1

(* Special cases *)
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

(* Concatenate remaining strings in a line  *)
let gather_strings (ts : token list) : string * token list =
  let rec aux (acc : string) (tl : token list) =
    match tl with Value s :: tl -> aux (acc ^ " " ^ s) tl | _ -> (acc, tl)
  in
  let string, tl = aux "" ts in
  (String.sub string ~pos:1 ~len:(String.length string - 1), tl)

(***********************************************************************************************)

(* Parse a single expression *)
let rec parse_expression (ts : token list) : e_context =
  (* Implements shunting yard algorithm for operator precedence *)
  let rec aux ts (es : expression list) (ops : binaryOp list) =
    match ts with
    (* function *)
    | Value fn :: Lparen :: tl ->
        let fn_call, tl = parse_fn_call ~fn tl in
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
  (* Assignment, no type annotation *)
  | Value name :: Assign :: tl ->
      let value, tl = parse_expression tl in
      let operator = None in
      (Assignment { name; t = Unknown; value; operator }, tl)
  | Value name :: Bop op :: Assign :: tl ->
      let value, tl = parse_expression tl in
      let operator = Some (map_bop op) in
      (Assignment { name; t = Unknown; value; operator }, tl)
  (* Assignment, w/ type annotation *)
  | Value name :: Colon :: ((IntDef | StringDef | BoolDef) as t) :: tl -> (
      let t = map_t t in
      match tl with
      | Assign :: tl ->
          let value, tl = parse_expression tl in
          let operator = None in
          (Assignment { name; t; value; operator }, tl)
      | Bop op :: Assign :: tl ->
          let value, tl = parse_expression tl in
          let operator = Some (map_bop op) in
          (Assignment { name; t; value; operator }, tl)
      | _ -> failwith "type annotation not followed by assignment")
  (* Not assignment *)
  | _ -> aux ts [] []

(* Parse everything after `name(` *)
and parse_fn_call ?(fn : string = "") (tl : token list) : e_context =
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
  ( (match fn with
    | "print" -> CoreFunctionCall { name = Print; arguments }
    | "input" -> CoreFunctionCall { name = Input; arguments }
    | _ -> FunctionCall { name = fn; arguments }),
    tl )

(***********************************************************************************************)

(* Parse a single statement *)
let rec parse_statement (ts : token list) : s_context =
  match ts with
  | Value "import" :: Value m :: tl -> (Import m, tl)
  | Hash :: tl ->
      let s, tl = gather_strings tl in
      (Comment s, tl)
  | FunDef :: Value name :: Lparen :: tl ->
      let parameters, return, tl = parse_fn_def tl in
      let body, tl = parse_body tl in
      (Function { name; parameters; body; return }, tl)
  | For :: Value v :: In :: Value _ :: Lparen :: tl -> parse_for tl v
  | ((While | If | Elif) as c) :: tl -> parse_conditional tl c
  | Else :: Colon :: Newline :: Indent :: tl ->
      let body, tl = parse_body tl in
      (Else { body }, tl)
  | Return :: tl ->
      let expression, tl = parse_expression tl in
      (Return expression, tl)
  | Lex.Break :: tl -> (Ast.Break, tl)
  | Lex.Continue :: tl -> (Ast.Continue, tl)
  | _ ->
      let expression, tl = parse_expression ts in
      (Expression expression, tl)

(* Parse everything after `def name(` *)
(* Could use fold_until, but we also need to return the tail... this way cleaner? *)
and parse_fn_def (ts : token list) : params * primitive * token list =
  let rec aux tl (ps : params) (ret_t : primitive) =
    match tl with
    | Value name :: Colon :: t :: tl -> aux tl ((name, map_t t) :: ps) ret_t
    | Colon :: Newline :: Indent :: tl -> (List.rev ps, ret_t, tl)
    | Arrow :: t :: tl -> aux tl ps (map_t t)
    | (Comma | Rparen) :: tl -> aux tl ps ret_t
    | _ ->
        failwith
          (sprintf "can't parse function declaration %s" (ts |> show_tokens))
  in
  aux ts [] Void

(* Parse everything after `for _ in range(` *)
and parse_for (ts : token list) (value : string) : s_context =
  let fn_call, tl = parse_fn_call ts in
  match tl with
  | Colon :: Newline :: Indent :: tl -> (
      let body, tl = parse_body tl in
      match fn_call with
      | FunctionCall { name = _; arguments = [ upper ] } ->
          ( For
              {
                value;
                lower = IntLiteral 0;
                upper;
                increment = IntLiteral 1;
                body;
              },
            tl )
      | FunctionCall { name = _; arguments = [ lower; upper ] } ->
          (For { value; lower; upper; increment = IntLiteral 1; body }, tl)
      | FunctionCall { name = _; arguments = [ lower; upper; increment ] } ->
          (For { value; lower; upper; increment; body }, tl)
      | _ ->
          failwith
            (sprintf "can't parse for-loop bounds %s"
               (fn_call |> showExpression)))
  | _ -> failwith (sprintf "can't parse for-loop %s" (ts |> show_tokens))

(* Parse everything after `while` *)
and parse_conditional (ts : token list) (c : token) : s_context =
  let test, tl = parse_expression ts in
  match tl with
  | Colon :: Newline :: Indent :: tl -> (
      let body, tl = parse_body tl in
      match c with
      | Lex.While -> (Ast.While { test; body }, tl)
      | Lex.If -> (Ast.If { test; body }, tl)
      | Lex.Elif -> (Ast.Elif { test; body }, tl)
      | _ -> impossible ())
  | _ -> failwith (sprintf "can't parse while-loop %s" (ts |> show_tokens))

(* Grab all the statements in a fixed scope *)
and parse_body (ts : token list) : a_context =
  let body_ts, tl = find_dedent ts in
  match parse body_ts with body, _ -> (body, tl)

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

(***********************************************************************************************)

(* Some modular helpers for post-processing operations *)

(* Apply f to every expression in an expression, plus itself *)
let deep_apply (e : expression) ~(f : expression -> expression) : expression =
  let rec aux (e : expression) : expression =
    (match e with
    | Assignment ({ name; t; value; operator } as r) ->
        Assignment { r with value = aux value }
    | BinaryOp ({ operator; left; right } as r) ->
        BinaryOp { r with left = aux left; right = aux right }
    | UnaryOp ({ operator; operand } as r) ->
        UnaryOp { r with operand = aux operand }
    | FunctionCall ({ name; arguments } as r) ->
        FunctionCall { r with arguments = List.map arguments ~f:aux }
    | CoreFunctionCall ({ name; arguments } as r) ->
        CoreFunctionCall { r with arguments = List.map arguments ~f:aux }
    | e -> e)
    |> f
  in
  aux e

(* Apply f to every expression in the ast *)
let map_expressions (ast : ast) ~(f : expression -> expression) ~(deep : bool) :
    ast =
  let f = if deep then deep_apply ~f else f in
  let rec aux (acc : ast) (ast : ast) : ast =
    match ast with
    | [] -> List.rev acc
    | Expression e :: tl -> aux (Expression (f e) :: acc) tl
    | Function ({ name; parameters; return; body } as r) :: tl ->
        aux (Function { r with body = aux [] body } :: acc) tl
    | Return e :: tl -> aux (Return (f e) :: acc) tl
    | While { test; body } :: tl ->
        aux (While { test = f test; body = aux [] body } :: acc) tl
    | If { test; body } :: tl ->
        aux (If { test = f test; body = aux [] body } :: acc) tl
    | Elif { test; body } :: tl ->
        aux (Elif { test = f test; body = aux [] body } :: acc) tl
    | Else { body } :: tl -> aux (Else { body = aux [] body } :: acc) tl
    | ((Pass | Break | Continue) as hd) :: tl -> aux (hd :: acc) tl
    | For ({ value; lower; upper; increment; body } as r) :: tl ->
        aux
          (For
             {
               r with
               lower = f lower;
               upper = f upper;
               increment = f increment;
               body = aux [] body;
             }
          :: acc)
          tl
    | hd :: tl -> aux (hd :: acc) tl
  in
  aux [] ast

(* Fold over all the statements in an ast, pre-order traversal *)
let rec fold_statements (ast : ast) ~(init : 'a) ~(f : 'a -> 'b -> 'a) : 'a =
  match ast with
  | [] -> init
  | hd :: tl -> (
      let init = f init hd in
      match hd with
      | Function { name; parameters; return; body } ->
          fold_statements tl ~init:(fold_statements body ~init ~f) ~f
      | For { value; lower; upper; increment; body } ->
          fold_statements tl ~init:(fold_statements body ~init ~f) ~f
      | While { test; body } | If { test; body } | Elif { test; body } ->
          fold_statements tl ~init:(fold_statements body ~init ~f) ~f
      | Else { body } ->
          fold_statements tl ~init:(fold_statements body ~init ~f) ~f
      | _ -> fold_statements tl ~init ~f)

(* CAUTION: MUTATION *)
(* NOTE: We _could_, in theory, do this monadically or by threading a table through function calls.
   However, we also want to continue updating this table as we DFS in our post-processing run.
   In my opinion, this way is more intuitive / simple, despite the use of side-effects. *)

(* If duplicate, overwrite with more recent *)
let add (tbl : type_tbl) (key : string) (data : primitive) : unit =
  Hashtbl.change tbl key ~f:(fun v -> match v with _ -> Some data)

let init_tbl (ast : ast) : type_tbl =
  let add_params tbl ps =
    List.fold ps ~init:() ~f:(fun _ (name, t) -> add tbl name t)
  in

  fold_statements ast
    ~init:(Hashtbl.create (module String))
    ~f:(fun tbl el ->
      match el with
      | Function { name; parameters; return = Unknown | Void; body } ->
          add_params tbl parameters;
          tbl
      | Function { name; parameters; return; body } ->
          add_params tbl parameters;
          add tbl name return;
          tbl
      | Expression (Assignment { name; t = Unknown | Void; value; operator }) ->
          tbl
      | Expression (Assignment { name; t; value; operator }) ->
          add tbl name t;
          tbl
      | _ -> tbl)

let infer_type ~(tbl : type_tbl) (e : expression) : expression =
  (* Naive way to reconcile differing types at leaves *)
  let reconcile a b : primitive option =
    match (a, b) with
    | Some at, Some bt when equal_primitive at bt -> Some at
    | Some String, Some _ | Some _, Some String -> Some String
    | Some Int, Some _ | Some _, Some Int -> Some Int
    | _ -> Some Boolean
  in

  (* DFS to leaves, and merge types *)
  let rec dfs e : primitive option =
    match e with
    | IntLiteral _ -> Some Int
    | StringLiteral _ -> Some String
    | BooleanLiteral _ -> Some Boolean
    | UnaryOp { operator; operand } -> dfs operand
    | BinaryOp { operator; left; right } -> reconcile (dfs left) (dfs right)
    | FunctionCall { name; arguments } -> Hashtbl.find tbl name
    | Identifier name -> Hashtbl.find tbl name
    | _ -> None
  in

  (* Match against assignments with unknown type *)
  match e with
  | Assignment ({ name; t = Unknown; value; operator = None } as r) -> (
      match dfs value with
      | Some t ->
          add tbl name t;
          Assignment { r with t }
      | None -> e)
  | _ -> e

(* Convert some UnaryOps to literals *)
let replace_neg (e : expression) : expression =
  match e with
  | UnaryOp { operator = Neg; operand = IntLiteral d } -> IntLiteral (-d)
  | _ -> e

(* Take global statements in python and place them in a main function *)
(* This isn't perfect, but necessary evil for interpreted to compiled language *)
let gather_main (ast : ast) : ast =
  let body, stripped_ast =
    List.partition_tf ast ~f:(fun s ->
        match s with
        | Expression _ | If _ | Elif _ | Else _ -> true
        | _ -> false)
  in

  stripped_ast
  @ [ Function { name = "main"; parameters = []; return = Int; body } ]

(* string -> raw ast *)
let to_raw_ast (s : string) : ast =
  match s |> tokenize |> parse with ast, _ -> ast

(* string -> processed ast *)
let to_ast (s : string) : ast =
  let raw_ast = to_raw_ast s in
  raw_ast
  |> map_expressions ~f:replace_neg ~deep:true
  |> map_expressions ~f:(infer_type ~tbl:(init_tbl raw_ast)) ~deep:false
  |> gather_main
