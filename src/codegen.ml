open Core

module type CodeGen = sig
  val convertToString : Ast.statement list -> string
end


(***********************************************************************************************)
module Expressions = struct
  (*CONVERSION of Expression*)

  let convertExpressionToString (exp : Ast.expression) main_tree : string =
    (*multiplication and division*)
    let rec multDiv left op right =
      match (Codegenutil.CodegenUtil.Common.checkIfSubAdd left, Codegenutil.CodegenUtil.Common.checkIfSubAdd right) with
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
            ( Codegenutil.CodegenUtil.Common.checkIfOrOperatorPresent left,
              Codegenutil.CodegenUtil.Common.checkIfOrOperatorPresent right )
          with
          | true, true ->
              "(" ^ mainHelper left ^ ") && (" ^ mainHelper right ^ ")"
          | true, false -> "(" ^ mainHelper left ^ ") && " ^ mainHelper right
          | false, true -> mainHelper left ^ " && (" ^ mainHelper right ^ ")"
          | false, false -> mainHelper left ^ " && " ^ mainHelper right)
      | Ast.Or -> (
          match
            ( Codegenutil.CodegenUtil.Common.checkIfAndOperatorPresent left,
              Codegenutil.CodegenUtil.Common.checkIfAndOperatorPresent right )
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
      | Ast.BooleanLiteral b -> Codegenutil.CodegenUtil.Common.convertBoolToString b
      | Ast.Identifier i ->  i
      (*Assignments*)
      | Ast.Assignment { name = id; value = exp; t = varType; operator = op } ->
          if Codegenutil.CodegenUtil.Common.is_variable_declared id then
            id ^ " " ^ Codegenutil.CodegenUtil.Common.binaryToString op ^ " " ^ mainHelper exp
          else (
            Codegenutil.CodegenUtil.Common.declare_variable id;
            Codegenutil.CodegenUtil.Common.primitiveToString varType ^ " " ^ id ^ " "
            ^ Codegenutil.CodegenUtil.Common.binaryToString op ^ " " ^ mainHelper exp)
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
          | Ast.Neg -> "-(" ^ mainHelper exp ^ ")")
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
              ^ Codegenutil.CodegenUtil.Common.getReturnType main_tree expList
              ^ ", "
              ^ String.concat ~sep:", " args
              ^ ")"
          | Input ->
              "scanf("
              ^ Codegenutil.CodegenUtil.Common.getReturnType main_tree expList
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
      Codegenutil.CodegenUtil.Common.clearHashTable ();
      numberOfTabs countTabs
      ^ Codegenutil.CodegenUtil.Common.primitiveToString prim
      ^ " " ^ name ^ "(" ^ Codegenutil.CodegenUtil.Common.convertArgsListString args ^ "){\n"
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
      | Ast.Import _m :: _tl -> failwith "TODO"
      | Ast.Comment _s :: _tl -> failwith "TODO"
    in

    helper main_tree "" 0
end

module GenerateHeader: CodeGen = struct
  
  let convertToString (main_tree : Ast.statement list) : string = 
    let rec helper (tree_list : Ast.statement list) (acc : string) : string =

      match tree_list with
      | [] -> acc
      | Ast.Function { name; parameters = args; return = prim; body = _ } :: tl ->
          helper tl (acc ^ Codegenutil.CodegenUtil.Common.primitiveToString prim ^ " " ^ name ^ "(" ^ Codegenutil.CodegenUtil.Common.convertArgsListString args ^ ");\n")
      | _ :: tl -> helper tl acc
     
    in
    helper main_tree ""

end