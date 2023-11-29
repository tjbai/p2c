open Core

module type CodeGen = sig
  val convertToString : Ast.statement list -> string
end

(*TODO
    Expressions
    Functions
    For Loops
    While Loops
    if
      Elif
  else
    Pass
    Break
    Conitinue
*)

module Expressions = struct end

module ConModule : CodeGen = struct
  (*HELPER FUNCTIONS*)

  (*check precedence for binary operations*)
  let checkIfSubAdd binaryOp =
    match binaryOp with
    | Ast.BinaryOp { operator = op; left = _; right = _ } -> (
        match op with Ast.Add -> true | Ast.Subtract -> true | _ -> false)
    | _ -> false

  (*check if an expression contains an integer*)
  let rec checkHasInt (exp : Ast.expression) : bool =
    match exp with
    | Ast.IntLiteral _ -> true
    | Ast.BinaryOp { operator = _; left; right } ->
        checkHasInt left || checkHasInt right
    | Ast.UnaryOp { operator = _; operand } -> checkHasInt operand
    | Ast.FunctionCall { name = _; arguments = expList } ->
        List.fold expList ~init:false ~f:(fun acc x -> acc || checkHasInt x)
    | Ast.CoreFunctionCall { name = _; arguments = expList } ->
        List.fold expList ~init:false ~f:(fun acc x -> acc || checkHasInt x)
    | _ -> false

  (*checks if an expression contains string*)
  let rec checkHasString (exp : Ast.expression) : bool =
    match exp with
    | Ast.StringLiteral _ -> true
    | Ast.BinaryOp { operator = _; left; right } ->
        checkHasString left || checkHasString right
    | Ast.UnaryOp { operator = _; operand } -> checkHasString operand
    | Ast.FunctionCall { name = _; arguments = expList } ->
        List.fold expList ~init:false ~f:(fun acc x -> acc || checkHasString x)
    | Ast.CoreFunctionCall { name = _; arguments = expList } ->
        List.fold expList ~init:false ~f:(fun acc x -> acc || checkHasString x)
    | _ -> false

  (*checks if an expression contains bool*)
  let rec checkHasBool (exp : Ast.expression) : bool =
    match exp with
    | Ast.BinaryOp { operator = _; left; right } ->
        checkHasBool left || checkHasBool right
    | Ast.UnaryOp { operator = _; operand } -> checkHasBool operand
    | Ast.FunctionCall { name = _; arguments = expList } ->
        List.fold expList ~init:false ~f:(fun acc x -> acc || checkHasBool x)
    | Ast.CoreFunctionCall { name = _; arguments = expList } ->
        List.fold expList ~init:false ~f:(fun acc x -> acc || checkHasBool x)
    | _ -> false

  (*converts bool to string - C type*)
  let convertBoolToString bool =
    match bool with true -> "True" | false -> "False"

  let primitiveToString input =
    match input with
    | Ast.Int -> "int"
    | Ast.String -> "string"
    | Ast.Boolean -> "bool"
    | _ -> ""

  (*CORE FUNCTIONS*)

  (*converts to core function call*)

  (*RETURN - e.g. return a + b*)
  let returnExpression (input : string) : string = "return " ^ input

  (*CONVERSION of Expression*)

  let convertExpressionToString (exp : Ast.expression) : string =
    let rec mainHelper (exp : Ast.expression) : string =
      match exp with
      | Ast.IntLiteral i -> string_of_int i
      | Ast.StringLiteral s -> "\"" ^ s ^ "\""
      | Ast.BooleanLiteral b -> convertBoolToString b
      | Ast.Identifier i -> i
      (*Assignments*)
      | Ast.Assignment { name = id; value = exp } -> id ^ " = " ^ mainHelper exp
      (*Binary Operations*)
      | Ast.BinaryOp { operator = op; left; right } -> (
          let multDiv left op right =
            match (checkIfSubAdd left, checkIfSubAdd right) with
            | true, true ->
                "(" ^ mainHelper left ^ ") " ^ op ^ " (" ^ mainHelper right
                ^ ")"
            | true, false ->
                "(" ^ mainHelper left ^ ") " ^ op ^ " " ^ mainHelper right
            | false, true ->
                mainHelper left ^ " " ^ op ^ " (" ^ mainHelper right ^ ")"
            | false, false ->
                mainHelper left ^ " " ^ op ^ " " ^ mainHelper right
          in
          match op with
          | Ast.Add -> mainHelper left ^ " + " ^ mainHelper right
          | Ast.Multiply -> multDiv left "*" right
          | Ast.Subtract -> mainHelper left ^ " - " ^ mainHelper right
          | Ast.Divide -> multDiv left "/" right
          | _ -> failwith "Catastrophic Error")
      (*Unary Operations*)
      | Ast.UnaryOp { operator = op; operand = exp } -> (
          match op with Ast.Not -> "!(" ^ mainHelper exp ^ ")")
      (*Functional Calls*)
      | Ast.FunctionCall { name = id; arguments = expList } ->
          let args = List.map expList ~f:mainHelper in
          id ^ "(" ^ String.concat ~sep:", " args ^ ")"
      (*Core Function Calls*)
      | Ast.CoreFunctionCall { name = id; arguments = expList } -> (
          let rec funcToString expList acc =
            match expList with
            | [] -> acc
            | hd :: tl -> (
                match checkHasInt hd with
                | true -> funcToString tl (acc ^ " %d")
                | false -> (
                    match checkHasString hd with
                    | true -> funcToString tl (acc ^ " %s")
                    | false -> (
                        match checkHasBool hd with
                        | true -> funcToString tl (acc ^ " %d")
                        | false -> failwith "Catastrophic Error")))
          in

          let args = List.map expList ~f:mainHelper in
          match id with
          | Print ->
              "printf(" ^ funcToString expList "" ^ ", "
              ^ String.concat ~sep:", " args
              ^ ")"
          | Input ->
              "scanf(" ^ funcToString expList "" ^ ", "
              ^ String.concat ~sep:",&" args
              ^ ")")
    in

    let result = mainHelper exp in
    result ^ ";\n"

  let convertArgsListString (argsList : (Ast.identifier * Ast.primitive) list) :
      string =
    let rec helper (argsList : (Ast.identifier * Ast.primitive) list) : string =
      match argsList with
      | [] -> ""
      | (id, prim) :: tl -> id ^ primitiveToString prim ^ ": " ^ helper tl
    in
    helper argsList

  let convertToString (tree_list : Ast.statement list) : string =
    let rec helper (tree_list : Ast.statement list) (acc : string) : string =
      match tree_list with
      | [] -> acc
      (*Expression Assignment*)
      | Ast.Expression exp :: tl -> helper tl (convertExpressionToString exp)
      (*Function Creation*)
      | Ast.Function
          { name; arguments = args; returnType = prim; body = stateList }
        :: tl ->
          let functionToString =
            primitiveToString prim ^ name ^ "(" ^ convertArgsListString args
            ^ "){\n\t" ^ helper stateList "" ^ "\n}"
          in
          helper tl functionToString
      (*Control statements*)
      | Ast.Return exp :: tl ->
          helper tl (returnExpression (convertExpressionToString exp)) ^ ";\n"
      | Ast.Pass :: tl -> helper tl (acc ^ "\treturn;\n")
      | Ast.Break :: tl -> helper tl (acc ^ "\tbreak;\n")
      | Ast.Continue :: tl -> helper tl (acc ^ "\tcontinue;\n")
      | _ -> "bob"
    in
    helper tree_list ""
end
