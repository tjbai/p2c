open Core

module type CodeGen = sig
  val convertToString : Ast.statement list -> string
end

(***********************************************************************************************)

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

  let declared_variables : (string, bool) Hashtbl.t = Hashtbl.create (module String)

  let is_variable_declared id =
    match Hashtbl.find declared_variables id with
    | Some _ -> true
    | None -> false

  let declare_variable id =
    Hashtbl.add_exn declared_variables ~key:id ~data:true
(* 
  let generateMapofVars (tree_list : Ast.statement list) :
      (string, Ast.primitive) Hashtbl.t =
    let map = Hashtbl.create (module String) in
    let rec helper (tree_list : Ast.statement list) : unit =
      match tree_list with
      | [] -> ()
      | Ast.Expression exp :: tl -> (
          match exp with
          | Ast.Assignment { name = id; value = _; t = prim; operator = _ } ->
              Hashtbl.add_exn map ~key:id ~data:prim;
              helper tl
          | _ -> helper tl)
      | _ :: tl -> helper tl
    in
    helper tree_list;
    map *)

  (*CORE FUNCTIONS*)

  (*Core Functions*)

  (*check precedence for binary operations*)
  let checkIfSubAdd binaryOp =
    match binaryOp with
    | Ast.BinaryOp { operator = op; left = _; right = _ } -> (
        match op with Ast.Add -> true | Ast.Subtract -> true | _ -> false)
    | _ -> false

  (*converts bool to string - C type*)
  let convertBoolToString bool =
    match bool with true -> "True" | false -> "False"

  let primitiveToString input =
    match input with
    | Ast.Int -> "int"
    | Ast.String -> "string"
    | Ast.Boolean -> "bool"
    | _ -> failwith "Invalid primitive"

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

module Expressions = struct
  (*CONVERSION of Expression*)

  let convertExpressionToString (exp : Ast.expression) main_tree : string =
    (*multiplication and division*)
    let rec multDiv left op right =
      match (Common.checkIfSubAdd left, Common.checkIfSubAdd right) with
      | true, true ->
          "(" ^ mainHelper left ^ ") " ^ op ^ " (" ^ mainHelper right ^ ")"
      | true, false ->
          "(" ^ mainHelper left ^ ") " ^ op ^ " " ^ mainHelper right
      | false, true ->
          mainHelper left ^ " " ^ op ^ " (" ^ mainHelper right ^ ")"
      | false, false -> mainHelper left ^ " " ^ op ^ " " ^ mainHelper right
    (*and or for pembdas purposes*)
    and andOrPemdas (left : Ast.expression) (right : Ast.expression)
        (op : Ast.binaryOp) : string =
      match op with
      | Ast.And -> (
          match
            ( Common.checkIfOrOperatorPresent left,
              Common.checkIfOrOperatorPresent right )
          with
          | true, true ->
              "(" ^ mainHelper left ^ ") && (" ^ mainHelper right ^ ")"
          | true, false -> "(" ^ mainHelper left ^ ") && " ^ mainHelper right
          | false, true -> mainHelper left ^ " && (" ^ mainHelper right ^ ")"
          | false, false -> mainHelper left ^ " && " ^ mainHelper right)
      | Ast.Or -> (
          match
            ( Common.checkIfAndOperatorPresent left,
              Common.checkIfAndOperatorPresent right )
          with
          | true, true ->
              "(" ^ mainHelper left ^ ") || (" ^ mainHelper right ^ ")"
          | true, false -> "(" ^ mainHelper left ^ ") || " ^ mainHelper right
          | false, true -> mainHelper left ^ " || (" ^ mainHelper right ^ ")"
          | false, false -> mainHelper left ^ " || " ^ mainHelper right)
      | _ -> failwith "Invalid operator"
    and mainHelper (exp : Ast.expression) : string =
      match exp with
      | Ast.IntLiteral i -> string_of_int i
      | Ast.StringLiteral s -> "\"" ^ s ^ "\""
      | Ast.BooleanLiteral b -> Common.convertBoolToString b
      | Ast.Identifier i ->  i
      (*Assignments*)
      | Ast.Assignment { name = id; value = exp; t = varType; operator = op } ->
          if Common.is_variable_declared id then
            id ^ " " ^ Common.binaryToString op ^ " " ^ mainHelper exp
          else (
            Common.declare_variable id;
            Common.primitiveToString varType ^ " " ^ id ^ " "
            ^ Common.binaryToString op ^ " " ^ mainHelper exp)
      (*Binary Operations*)
      | Ast.BinaryOp { operator = op; left; right } -> (
          match op with
          | Ast.Add -> mainHelper left ^ " + " ^ mainHelper right
          | Ast.Multiply -> multDiv left "*" right
          | Ast.Subtract -> mainHelper left ^ " - " ^ mainHelper right
          | Ast.Divide -> multDiv left "/" right
          | Ast.And -> andOrPemdas left right op
          | Ast.Or -> andOrPemdas left right op
          | Ast.Equal -> mainHelper left ^ " == " ^ mainHelper right
          | Ast.NotEqual -> mainHelper left ^ " != " ^ mainHelper right
          | Ast.Lt -> mainHelper left ^ " < " ^ mainHelper right
          | Ast.Lte -> mainHelper left ^ " <= " ^ mainHelper right
          | Ast.Gt -> mainHelper left ^ " > " ^ mainHelper right
          | Ast.Gte -> mainHelper left ^ " >= " ^ mainHelper right
          | Ast.Mod -> mainHelper left ^ " % " ^ mainHelper right)
      (*Unary Operations*)
      | Ast.UnaryOp { operator = op; operand = exp } -> (
          match op with
          | Ast.Not -> "!(" ^ mainHelper exp ^ ")"
          | Ast.Neg -> "-(" ^ mainHelper exp ^ ")"
        )
      (*Functional Calls*)
      | Ast.FunctionCall { name = id; arguments = expList } ->
          let args = List.map expList ~f:mainHelper in
          id ^ "(" ^ String.concat ~sep:", " args ^ ")"
      (*Core Function Calls*)
      | Ast.CoreFunctionCall { name = id; arguments = expList } -> (
          let args = List.map expList ~f:mainHelper in
          match id with
          | Print ->
              "printf("
              ^ Common.getReturnType main_tree expList
              ^ ", "
              ^ String.concat ~sep:", " args
              ^ ")"
          | Input ->
              "scanf("
              ^ Common.getReturnType main_tree expList
              ^ ", "
              ^ String.concat ~sep:",&" args
              ^ ")")
    in

    let result = mainHelper exp in
    result
end

(***********************************************************************************************)
module ConModule : CodeGen = struct
  (*CORE FUNCTIONS*)

  (*RETURN - e.g. return a + b*)
  let returnExpression (input : string) : string = "return " ^ input

  (*Convert arguments of function to string*)
  let convertArgsListString (argsList : (string * Ast.primitive) list) : string
      =
    let rec helper (argsList : (string * Ast.primitive) list) : string =
      match argsList with
      | [] -> ""
      | (id, prim) :: tl -> Common.primitiveToString prim ^ " " ^ id ^ helper tl
    in
    helper argsList

  let numberOfTabs (num : int) : string =
    let rec helper (num : int) (acc : string) : string =
      match num with 0 -> acc | _ -> helper (num - 1) acc ^ "\t"
    in
    helper num ""

  (*Convert for loop to string*)
  let convertForLoopString (value : string) (lower : string) (upper : string)
      (increment : string) =
    "for(int " ^ value ^ "=" ^ lower ^ ";" ^ value ^ "<" ^ upper ^ ";" ^ value
    ^ "=" ^ value ^ "+" ^ increment ^ ")"

  let convertToString (main_tree : Ast.statement list) : string =
    (*function to string*)
    let rec functionToString prim name args stateList countTabs =
      numberOfTabs countTabs
      ^ Common.primitiveToString prim
      ^ " " ^ name ^ "(" ^ convertArgsListString args ^ "){\n"
      ^ helper stateList "" (countTabs + 1)
      ^ "}"
    (*for loop conversion*)
    and forLoopStr id lower upper inc statelist countTabs =
      numberOfTabs countTabs
      ^ convertForLoopString id
          (Expressions.convertExpressionToString lower main_tree)
          (Expressions.convertExpressionToString upper main_tree)
          (Expressions.convertExpressionToString inc main_tree)
      ^ "{\n"
      ^ helper statelist "" (countTabs + 1)
      ^ "}"
    (*while loop conversion*)
    and whileLoopStr exp statelist countTabs =
      numberOfTabs countTabs ^ "while("
      ^ Expressions.convertExpressionToString exp main_tree
      ^ "){\n"
      ^ helper statelist "" (countTabs + 1)
      ^ "}"
    (*if statement conversion*)
    and ifStr exp statelist countTabs =
      numberOfTabs countTabs ^ "if("
      ^ Expressions.convertExpressionToString exp main_tree
      ^ "){\n"
      ^ helper statelist "" (countTabs + 1)
      ^ "}"
    (*else if statement conversion*)
    and elifStr exp statelist countTabs =
      numberOfTabs countTabs ^ "else if("
      ^ Expressions.convertExpressionToString exp main_tree
      ^ ") {\n"
      ^ helper statelist "" (countTabs + 1)
      ^ "}"
    (*else string conversions*)
    and elseStr statelist countTabs =
      numberOfTabs countTabs ^ "else {\n"
      ^ helper statelist "" (countTabs + 1)
      ^ "}"
    and helper (tree_list : Ast.statement list) (acc : string) (countTabs : int)
        : string =
      match tree_list with
      | [] -> acc
      (*Expression Assignment*)
      | Ast.Expression exp :: tl ->
          helper tl
            (acc ^ numberOfTabs countTabs
            ^ Expressions.convertExpressionToString exp main_tree
            ^ ";\n")
            countTabs
      (*Function Creation*)
      | Ast.Function
          { name; parameters = args; return = prim; body = stateList }
        :: tl ->

          helper tl
            (acc ^ functionToString prim name args stateList countTabs )
            countTabs
      (*For Loops*)
      | Ast.For { value = id; increment = inc; lower; upper; body = statelist }
        :: tl ->
          helper tl
            (acc ^ forLoopStr id lower upper inc statelist countTabs)
            countTabs
      (*while Loops*)
      | Ast.While { test = exp; body = statelist } :: tl ->
          helper tl (acc ^ whileLoopStr exp statelist countTabs) countTabs
      (*if statements*)
      | Ast.If { test = exp; body = statelist } :: tl ->
          helper tl (acc ^ ifStr exp statelist countTabs) countTabs
      (*else if statements*)
      | Ast.Elif { test = exp; body = statelist } :: tl ->
          helper tl (acc ^ elifStr exp statelist countTabs) countTabs
      (*else statements*)
      | Ast.Else { body = statelist } :: tl ->
          helper tl (acc ^ elseStr statelist countTabs) countTabs
      (*Control statements*)
      | Ast.Return exp :: tl ->
          numberOfTabs countTabs
          ^ helper tl
              (returnExpression
                 (Expressions.convertExpressionToString exp main_tree))
              countTabs
          ^ ";\n"
      | Ast.Pass :: tl ->
          numberOfTabs countTabs ^ helper tl (acc ^ "return;\n") countTabs
      | Ast.Break :: tl ->
          numberOfTabs countTabs ^ helper tl (acc ^ "break;\n") countTabs
      | Ast.Continue :: tl ->
          numberOfTabs countTabs ^ helper tl (acc ^ "continue;\n") countTabs
    in

    helper main_tree "" 0
end
