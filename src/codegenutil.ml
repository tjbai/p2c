open Core

module FunctionLookUp = struct
  module M = Map.Make (String)

  let generateMapOfFunctionSignatures (tree_list : Ast.statement list) =
    let rec helper (tree_list : Ast.statement list) map =
      match tree_list with
      | [] -> map
      | Ast.Function { name; parameters = args; return = prim; body = _ } :: tl
        ->
          helper tl (Map.add_exn map ~key:name ~data:(prim, args))
      | _ :: tl -> helper tl map
    in
    helper tree_list M.empty

  let findReturnType (id : string) tree : Ast.primitive =
    let map = generateMapOfFunctionSignatures tree in
    match Map.find map id with
    | Some (prim, _) -> prim
    | None -> failwith "Function not found"
end

(***********************************************************************************************)

module Common = struct
  (*HELPER FUNCTIONS*)

  let declared_variables : (string, bool) Hashtbl.t =
    Hashtbl.create (module String)

  let is_variable_declared id =
    match Hashtbl.find declared_variables id with
    | Some _ -> true
    | None -> false

  let declare_variable id =
    Hashtbl.add_exn declared_variables ~key:id ~data:true

  let clear () = Hashtbl.clear declared_variables

  (*CORE FUNCTIONS*)

  (*check precedence for binary operations*)
  let checkIfSubAdd binaryOp =
    match binaryOp with
    | Ast.BinaryOp { operator = op; left = _; right = _ } -> (
        match op with Ast.Add -> true | Ast.Subtract -> true | Ast.And -> true | Ast.Or -> true | _ -> false)
    | _ -> false
  
  let checkAndOr binaryOp = 
    match binaryOp with
    | Ast.BinaryOp { operator = op; left = _; right = _ } -> (
        match op with Ast.And -> true | Ast.Or -> true | _ -> false)
    | _ -> false

  (*converts bool to string - C type*)
  let convertBoolToString bool =
    match bool with true -> "true" | false -> "false"

  (*check if else statement comes next*)
  let checkIfElseStatementNext (stmt : Ast.statement list) : bool =
    match stmt with
    | [] -> false
    | Ast.Elif { test = _; body = _ } :: _ -> true
    | Ast.Else { body = _ } :: _ -> true
    | _ -> false

  let primitiveToString input =
    match input with
    | Ast.Void -> "void"
    | Ast.Int -> "int"
    | Ast.String -> "string"
    | Ast.Boolean -> "bool"
    | _ -> ""
  
  let primitiveFuncToString input =
    match input with
    | Ast.Void -> "void"
    | Ast.Int -> "int"
    | Ast.String -> "string"
    | Ast.Boolean -> "bool"
    | _ -> ""

  let rec checkIfAndOperatorPresent (exp : Ast.expression) : bool =
    match exp with
    | Ast.BinaryOp { operator = op; left; right } -> (
        match op with
        | Ast.And -> true
        | _ -> checkIfAndOperatorPresent left || checkIfAndOperatorPresent right
        )
    | _ -> false

  let rec checkIfOrOperatorPresent (exp : Ast.expression) : bool =
    match exp with
    | Ast.BinaryOp { operator = op; left; right } -> (
        match op with
        | Ast.Or -> true
        | _ -> checkIfOrOperatorPresent left || checkIfOrOperatorPresent right)
    | _ -> false

  let getReturnType tree_main expList =
    let rec helper expList acc =
      match expList with
      | [] -> acc
      | hd :: tl -> (
          match hd with
          | Ast.IntLiteral _ -> helper tl (acc ^ "%d ")
          | Ast.StringLiteral _ -> helper tl (acc ^ "%s ")
          | Ast.BooleanLiteral _ -> helper tl (acc ^ "%d ")
          | Ast.FunctionCall { name = id; arguments = _ } -> (
              match FunctionLookUp.findReturnType id tree_main with
              | Ast.Int -> helper tl (acc ^ "%d ")
              | Ast.String -> helper tl (acc ^ "%s ")
              | Ast.Boolean -> helper tl (acc ^ "%d ")
              | _ -> failwith "Sprintf | Printf: Invalid Return type")
          | _ -> helper tl acc)
    in
    helper expList ""

  let convertArgsListString (argsList : (string * Ast.primitive) list) : string
      =
    let rec helper (argsList : (string * Ast.primitive) list) : string =
      match argsList with
      | [] -> ""
      | (id, prim) :: tl -> primitiveToString prim ^ " " ^ id ^ ", " ^ helper tl
    in

    let result = helper argsList in

    if String.length result = 0 then result
    else String.sub result ~pos:0 ~len:(String.length result - 2)

  let binaryToString (op : Ast.binaryOp option) : string =
    match op with
    | Some op -> (
        match op with
        | Ast.Add -> "+"
        | Ast.Multiply -> "*"
        | Ast.Subtract -> "-"
        | Ast.Divide -> "/"
        | Ast.And -> "&&"
        | Ast.Or -> "||"
        | Ast.Equal -> "=="
        | Ast.NotEqual -> "!="
        | Ast.Lt -> "<"
        | Ast.Lte -> "<="
        | Ast.Gt -> ">"
        | Ast.Gte -> ">="
        | Ast.Mod -> "%")
    | None -> "="
end
