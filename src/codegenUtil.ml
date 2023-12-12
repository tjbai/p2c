open Core

module FunctionLookUp = struct
  let generateMapOfFunctionSignatures (tree_list : Ast.statement list) :
      (string, Ast.primitive * (string * Ast.primitive) list) Hashtbl.t =
    let map = Hashtbl.create (module String) in
    let rec helper (tree_list : Ast.statement list) : unit =
      match tree_list with
      | [] -> ()
      | Ast.Function { name; parameters = args; return = prim; body = _ } :: tl
        ->
          Hashtbl.add_exn map ~key:name ~data:(prim, args);
          helper tl
      | _ :: tl -> helper tl
    in
    helper tree_list;
    map

  let findReturnType (id : string) tree : Ast.primitive =
    let hashtbl = generateMapOfFunctionSignatures tree in
    match Hashtbl.find hashtbl id with
    | Some (prim, _) -> prim
    | None -> Ast.Void
end

(***********************************************************************************************)

module Common = struct
  (*HELPER FUNCTIONS*)

  (*Convert arguments of function to string*)

  let declared_variables : (string, bool) Hashtbl.t =
    Hashtbl.create (module String)

  let is_variable_declared id =
    match Hashtbl.find declared_variables id with
    | Some _ -> true
    | None -> false

  let declare_variable id =
    Hashtbl.add_exn declared_variables ~key:id ~data:true

  let clearHashTable () = Hashtbl.clear declared_variables

  (*CORE FUNCTIONS*)

  (*check precedence for binary operations*)
  let checkIfSubAdd binaryOp =
    match binaryOp with
    | Ast.BinaryOp { operator = op; left = _; right = _ } -> (
        match op with Ast.Add -> true | Ast.Subtract -> true | _ -> false)
    | _ -> false

  (*converts bool to string - C type*)
  let convertBoolToString bool =
    match bool with true -> "true" | false -> "false"

  let primitiveToString input =
    match input with
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
              | _ -> failwith "Invalid type")
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
