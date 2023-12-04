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

module Common = struct
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
end

module Expressions = struct
  (*CONVERSION of Expression*)

  let convertExpressionToString (exp : Ast.expression) : string =
    let rec mainHelper (exp : Ast.expression) : string =
      match exp with
      | Ast.IntLiteral i -> string_of_int i
      | Ast.StringLiteral s -> "\"" ^ s ^ "\""
      | Ast.BooleanLiteral b -> Common.convertBoolToString b
      | Ast.Identifier i -> i
      (*Assignments*)
      | Ast.Assignment { name = id; value = exp; t = varType } ->
          Common.primitiveToString varType ^ " " ^ id ^ " = " ^ mainHelper exp
      (*Binary Operations*)
      | Ast.BinaryOp { operator = op; left; right } -> (
          let multDiv left op right =
            match (Common.checkIfSubAdd left, Common.checkIfSubAdd right) with
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
          | Ast.And -> (
              match
                ( Common.checkIfOrOperatorPresent left,
                  Common.checkIfOrOperatorPresent right )
              with
              | true, true ->
                  "(" ^ mainHelper left ^ ") && (" ^ mainHelper right ^ ")"
              | true, false ->
                  "(" ^ mainHelper left ^ ") && " ^ mainHelper right
              | false, true ->
                  mainHelper left ^ " && (" ^ mainHelper right ^ ")"
              | false, false -> mainHelper left ^ " && " ^ mainHelper right)
          | Ast.Or -> (
              match
                ( Common.checkIfAndOperatorPresent left,
                  Common.checkIfAndOperatorPresent right )
              with
              | true, true ->
                  "(" ^ mainHelper left ^ ") || (" ^ mainHelper right ^ ")"
              | true, false ->
                  "(" ^ mainHelper left ^ ") || " ^ mainHelper right
              | false, true ->
                  mainHelper left ^ " || (" ^ mainHelper right ^ ")"
              | false, false -> mainHelper left ^ " || " ^ mainHelper right)
          | Ast.Equal -> mainHelper left ^ " == " ^ mainHelper right
          | Ast.NotEqual -> mainHelper left ^ " != " ^ mainHelper right
          | Ast.Lt -> mainHelper left ^ " < " ^ mainHelper right
          | Ast.Lte -> mainHelper left ^ " <= " ^ mainHelper right
          | Ast.Gt -> mainHelper left ^ " > " ^ mainHelper right
          | Ast.Gte -> mainHelper left ^ " >= " ^ mainHelper right)
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
                match Common.checkHasInt hd with
                | true -> funcToString tl (acc ^ " %d")
                | false -> (
                    match Common.checkHasString hd with
                    | true -> funcToString tl (acc ^ " %s")
                    | false -> (
                        match Common.checkHasBool hd with
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
              ^ ")"
          | Range ->
              if List.length expList = 2 then "5"
              else if List.length expList = 3 then "8"
              else "whatever")
    in

    let result = mainHelper exp in
    result
end

module ConModule : CodeGen = struct
  (*CORE FUNCTIONS*)

  (*converts to core function call*)

  (*RETURN - e.g. return a + b*)
  let returnExpression (input : string) : string = "return " ^ input

  let convertArgsListString (argsList : (string * Ast.primitive) list) : string
      =
    let rec helper (argsList : (string * Ast.primitive) list) : string =
      match argsList with
      | [] -> ""
      | (id, prim) :: tl -> Common.primitiveToString prim ^ " " ^ id ^ helper tl
    in
    helper argsList

  let convertForLoopString value lower upper increment =
    "for(int " ^ value ^ "=" ^ string_of_int lower ^ ";" ^ value ^ "<"
    ^ string_of_int upper ^ ";" ^ value ^ "=" ^ value ^ "+"
    ^ string_of_int increment ^ ")"

  let convertToString (tree_list : Ast.statement list) : string =
    let rec helper (tree_list : Ast.statement list) (acc : string) : string =
      match tree_list with
      | [] -> acc
      (*Expression Assignment*)
      | Ast.Expression exp :: tl ->
          helper tl acc ^ Expressions.convertExpressionToString exp
      (*Function Creation*)
      | Ast.Function
          { name; parameters = args; return = prim; body = stateList }
        :: tl ->
          let functionToString =
            Common.primitiveToString prim
            ^ " " ^ name ^ "(" ^ convertArgsListString args ^ "){\n\t"
            ^ helper stateList ";\n" ^ "}"
          in
          helper tl acc ^ functionToString
      (*For Loops*)
      | Ast.For { value = id; increment = inc; lower; upper; body = statelist }
        :: tl ->
          let forLoopStr =
            convertForLoopString id lower upper inc
            ^ "{\n\t" ^ helper statelist "" ^ ";\n}"
          in
          helper tl acc ^ forLoopStr
      (*while Loops*)
      | Ast.While { test = exp; body = statelist } :: tl ->
          let whileLoopStr =
            "while("
            ^ Expressions.convertExpressionToString exp
            ^ "){\n\t" ^ helper statelist "" ^ ";\n}"
          in
          helper tl acc ^ whileLoopStr
      (*if statements*)
      | Ast.If { test = exp; body = statelist } :: tl ->
          let ifStr =
            "if("
            ^ Expressions.convertExpressionToString exp
            ^ "){\n\t" ^ helper statelist "" ^ ";\n}"
          in
          helper tl acc ^ ifStr
      (*else if statements*)
      | Ast.Elif { test = exp; body = statelist } :: tl ->
          let elifStr =
            "else if("
            ^ Expressions.convertExpressionToString exp
            ^ "){\n\t" ^ helper statelist "" ^ ";\n}"
          in
          helper tl acc ^ elifStr
      (*else statements*)
      | Ast.Else { body = statelist } :: tl ->
          let elseStr = "else{\n\t" ^ helper statelist "" ^ ";\n}" in
          helper tl acc ^ elseStr
      (*Control statements*)
      | Ast.Return exp :: tl ->
          helper tl
            (returnExpression (Expressions.convertExpressionToString exp))
          ^ ";\n"
      | Ast.Pass :: tl -> helper tl (acc ^ "\treturn;")
      | Ast.Break :: tl -> helper tl (acc ^ "\tbreak;\n")
      | Ast.Continue :: tl -> helper tl (acc ^ "\tcontinue;\n")
    in
    helper tree_list ""
end
