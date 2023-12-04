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
  let convertForLoopString value lower upper increment =
    "for(int " ^ value ^ "=" ^ string_of_int lower ^ ";" ^ value ^ "<"
    ^ string_of_int upper ^ ";" ^ value ^ "=" ^ value ^ "+"
    ^ string_of_int increment ^ ")"

  let convertToString (tree_list : Ast.statement list) : string =
    let rec helper (tree_list : Ast.statement list) (acc : string)
        (countTabs : int) : string =
      match tree_list with
      | [] -> acc
      (*Expression Assignment*)
      | Ast.Expression exp :: tl ->
          helper tl
            (acc ^ numberOfTabs countTabs
            ^ Expressions.convertExpressionToString exp
            ^ ";\n")
            countTabs
      (*Function Creation*)
      | Ast.Function
          { name; parameters = args; return = prim; body = stateList }
        :: tl ->
          let functionToString =
            numberOfTabs countTabs
            ^ Common.primitiveToString prim
            ^ " " ^ name ^ "(" ^ convertArgsListString args ^ "){\n"
            ^ helper stateList "" (countTabs + 1)
            ^ "}"
          in
          helper tl (acc ^ functionToString) countTabs
      (*For Loops*)
      | Ast.For { value = id; increment = inc; lower; upper; body = statelist }
        :: tl ->
          let forLoopStr =
            numberOfTabs countTabs
            ^ convertForLoopString id lower upper inc
            ^ "{\n"
            ^ helper statelist "" (countTabs + 1)
            ^ "}"
          in
          helper tl (acc ^ forLoopStr) countTabs
      (*while Loops*)
      | Ast.While { test = exp; body = statelist } :: tl ->
          let whileLoopStr =
            numberOfTabs countTabs ^ "while("
            ^ Expressions.convertExpressionToString exp
            ^ "){\n"
            ^ helper statelist "" (countTabs + 1)
            ^ "}"
          in
          helper tl (acc ^ whileLoopStr) countTabs
      (*if statements*)
      | Ast.If { test = exp; body = statelist } :: tl ->
          let ifStr =
            numberOfTabs countTabs ^ "if("
            ^ Expressions.convertExpressionToString exp
            ^ "){\n"
            ^ helper statelist "" (countTabs + 1)
            ^ "}"
          in
          helper tl (acc ^ ifStr) countTabs
      (*else if statements*)
      | Ast.Elif { test = exp; body = statelist } :: tl ->
          let elifStr =
            numberOfTabs countTabs ^ "else if("
            ^ Expressions.convertExpressionToString exp
            ^ ") {\n"
            ^ helper statelist "" (countTabs + 1)
            ^ "}"
          in
          helper tl (acc ^ elifStr) countTabs
      (*else statements*)
      | Ast.Else { body = statelist } :: tl ->
          let elseStr =
            numberOfTabs countTabs ^ "else {\n"
            ^ helper statelist "" (countTabs + 1)
            ^ "}"
          in
          helper tl (acc ^ elseStr) countTabs
      (*Control statements*)
      | Ast.Return exp :: tl ->
          numberOfTabs countTabs
          ^ helper tl
              (returnExpression (Expressions.convertExpressionToString exp))
              countTabs
          ^ ";\n"
      | Ast.Pass :: tl ->
          numberOfTabs countTabs ^ helper tl (acc ^ "return;") countTabs
      | Ast.Break :: tl ->
          numberOfTabs countTabs ^ helper tl (acc ^ "break;\n") countTabs
      | Ast.Continue :: tl ->
          numberOfTabs countTabs ^ helper tl (acc ^ "continue;\n") countTabs
    in
    helper tree_list "" 0
end
