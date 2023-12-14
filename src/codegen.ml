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

      let leftText = mainHelper left in
      let rightText = mainHelper right in
      match
        ( Codegenutil.Common.checkIfSubAdd left,
          Codegenutil.Common.checkIfSubAdd right )
      with
      | true, true ->
        Format.sprintf "( %s ) %s ( %s )\n" leftText op rightText;
      | true, false ->
        Format.sprintf"( %s ) %s %s\n" leftText op rightText;
      | false, true ->
        Format.sprintf "%s %s ( %s )\n" leftText op rightText;
      | false, false -> Format.sprintf "%s %s %s\n" leftText op rightText;
    (*and or for pembdas purposes*)
    and andOrPemdas (left : Ast.expression) (right : Ast.expression)
        (op : Ast.binaryOp) : string =
        let leftText = mainHelper left in
        let rightText = mainHelper right in
      match op with
      | Ast.And -> (
          match
            ( Codegenutil.Common.checkIfOrOperatorPresent left,
              Codegenutil.Common.checkIfOrOperatorPresent right )
          with
          | true, true ->
            Format.sprintf "( %s ) && ( %s )\n" leftText rightText
          | true, false -> Format.sprintf "( %s ) && %s\n" leftText rightText
          | false, true -> Format.sprintf "%s && ( %s )\n" leftText rightText
          | false, false -> Format.sprintf "%s && %s\n" leftText rightText)
      | Ast.Or -> (
        let leftText = mainHelper left in
        let rightText = mainHelper right in
          match
            ( Codegenutil.Common.checkIfAndOperatorPresent left,
              Codegenutil.Common.checkIfAndOperatorPresent right )
          with
          | true, true ->
            Format.sprintf "( %s ) || ( %s )\n" leftText rightText
          | true, false -> Format.sprintf "( %s ) || %s\n" leftText rightText
          | false, true -> Format.sprintf "%s || ( %s )\n" leftText rightText
          | false, false -> Format.sprintf "%s || %s\n" leftText rightText)
      | _ -> failwith "Invalid operator"
    and mainHelper (exp : Ast.expression) : string =
      match exp with
      | Ast.IntLiteral i -> string_of_int i
      | Ast.StringLiteral s -> "\"" ^ s ^ "\""
      | Ast.BooleanLiteral b -> Codegenutil.Common.convertBoolToString b
      | Ast.Identifier i -> i
      (*Assignments*)
      | Ast.Assignment { name = id; value = exp; t = varType; operator = op } ->
          (*Need to consider edge case += etc*)
          let checkToAddEquals = 
            match Codegenutil.Common.binaryToString op with 
            | "=" -> ""
            | _ -> "="
          in
          if Codegenutil.Common.is_variable_declared id then
            id ^ " "
            ^ Codegenutil.Common.binaryToString op
            ^ " " ^ mainHelper exp
          else (
            Codegenutil.Common.declare_variable id;
            Codegenutil.Common.primitiveToString varType
            ^ " " ^ id ^ " "
            ^ (checkToAddEquals)^Codegenutil.Common.binaryToString op
            ^ " " ^ mainHelper exp) 
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

          (*need to filter out external functions*)
          let filteredOutName = 
            if String.contains id '.' then 
              String.split id ~on:'.'
              |> List.last_exn
            else id
          in

          let args = List.map expList ~f:mainHelper in
          filteredOutName ^ "(" ^ String.concat ~sep:", " args ^ ")"
      (*Core Function Calls*)
      | Ast.CoreFunctionCall { name = id; arguments = expList } -> (
          let args = List.map expList ~f:mainHelper in
          match id with
          | Print ->
            Format.sprintf "printf(\"%s\", %s)" (Codegenutil.Common.getReturnType main_tree expList) (String.concat ~sep:", " args)
          | Input ->
            Format.sprintf "scanf(\"%s\", %s)" (Codegenutil.Common.getReturnType main_tree expList) (String.concat ~sep:",&" args))

    in

    let result = mainHelper exp in
    result
end

(***********************************************************************************************)
module ConModule : CodeGen = struct
  (*CORE FUNCTIONS*)
  let numberOfTabs (num : int) : string =
    String.make num '\t'
  (*RETURN - e.g. return a + b*)
  let returnExpression (input : string) (countTabs: int) : string = (numberOfTabs countTabs )^"return " ^ input

  (*Convert for loop to string*)
  let convertForLoopString (value : string) (lower : string) (upper : string)
      (increment : string) =
    Format.sprintf "for(int %s = %s; %s <= %s; %s += %s)" value lower value
      upper value increment

  let convertToString (main_tree : Ast.statement list) : string =
    (*function to string*)
    let rec functionToString prim name args stateList countTabs =
      numberOfTabs countTabs ^ Format.sprintf "%s %s(%s){\n%s%s}\n" (Codegenutil.Common.primitiveToString prim) name
        (Codegenutil.Common.convertArgsListString args)
        (helper stateList "" (countTabs + 1))
        (numberOfTabs countTabs)
    (*for loop conversion*)
    and forLoopStr id lower upper inc statelist countTabs =
      numberOfTabs countTabs
      ^ convertForLoopString id
          (Expressions.convertExpressionToString lower main_tree)
          (Expressions.convertExpressionToString upper main_tree)
          (Expressions.convertExpressionToString inc main_tree)
      ^ "{\n"
      ^ helper statelist "" (countTabs + 1)
      ^ numberOfTabs countTabs ^ "}\n"
    (*while loop conversion*)
    and whileLoopStr exp statelist countTabs =
      numberOfTabs countTabs ^ Format.sprintf "while(%s){\n%s%s}\n"
        (Expressions.convertExpressionToString exp main_tree)
        (helper statelist "" (countTabs + 1))
        (numberOfTabs countTabs)
    (*if statement conversion*)
    and ifStr exp statelist countTabs =
      numberOfTabs countTabs ^ Format.sprintf "if(%s){\n%s%s}\n"
        (Expressions.convertExpressionToString exp main_tree)
        (helper statelist "" (countTabs + 1))
        (numberOfTabs countTabs)
    (*else if statement conversion*)
    and elifStr exp statelist countTabs =
      numberOfTabs countTabs ^ Format.sprintf "else if(%s){\n%s%s}\n"
        (Expressions.convertExpressionToString exp main_tree)
        (helper statelist "" (countTabs + 1))
        (numberOfTabs countTabs)
    (*else string conversions*)
    and elseStr statelist countTabs =
      Format.sprintf "%selse{\n%s%s}\n" (numberOfTabs countTabs)
        (helper statelist "" (countTabs + 1))
        (numberOfTabs countTabs)
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
          (*if the main function is empty then we ignore*)
          if String.equal name "main" && List.is_empty stateList then
            helper tl acc countTabs
          else
            helper tl
              (acc ^ functionToString prim name args stateList countTabs)
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
        let addNewLineIfElse = 
          match Codegenutil.Common.checkIfElseStatementNext tl with
          | true -> ""
          | false -> "\n"
        in
          helper tl ((acc ^ ifStr exp statelist countTabs)^"}"^addNewLineIfElse) countTabs
      (*else if statements*)
      | Ast.Elif { test = exp; body = statelist } :: tl ->
          helper tl (acc ^ elifStr exp statelist countTabs) countTabs
      (*else statements*)
      | Ast.Else { body = statelist } :: tl ->
          helper tl (acc ^ elseStr statelist countTabs) countTabs
      (*Control statements*)
      | Ast.Return exp :: tl ->
          helper tl
          (acc^(returnExpression
                 (Expressions.convertExpressionToString exp main_tree) countTabs))
              countTabs
          ^ ";\n"
      | Ast.Pass :: tl ->
          numberOfTabs countTabs ^ helper tl (acc ^ "return;\n") countTabs
      | Ast.Break :: tl ->
          numberOfTabs countTabs ^ helper tl (acc ^ "break;\n") countTabs
      | Ast.Continue :: tl ->
          numberOfTabs countTabs ^ helper tl (acc ^ "continue;\n") countTabs
      | Ast.Import _ :: tl -> helper tl acc countTabs
      | Ast.Comment _s :: _tl ->
          numberOfTabs countTabs ^ helper _tl (acc ^ "//" ^ _s ^ "\n") countTabs
    in

    helper main_tree "" 0
end

module GenerateHeader : CodeGen = struct
  let commonCLibraries =
    [ "stdio.h"; "stdlib.h"; "stdbool.h"; "string.h"; "math.h" ]

  let commonCLibrariesToString (libraries : string list) : string =
    let rec helper (libraries : string list) (acc : string) : string =
      match libraries with
      | [] -> acc
      | hd :: tl -> helper tl (acc ^ "#include <" ^ hd ^ ">\n")
    in
    helper libraries ""

  let convertToString (main_tree : Ast.statement list) : string =
    let rec helper (tree_list : Ast.statement list) (acc : string) : string =
      match tree_list with
      | [] -> acc
      | Ast.Import s :: tl -> helper tl (acc ^ "#include \"" ^ s ^ ".h\"\n")
      | Ast.Function { name; parameters = args; return = prim; body = bdy }
        :: tl ->
          (*if the main function is empty*)
          if String.equal name "main" && List.is_empty bdy then helper tl acc
          else
            helper tl
              (acc
              ^ Codegenutil.Common.primitiveToString prim
              ^ " " ^ name ^ "("
              ^ Codegenutil.Common.convertArgsListString args
              ^ ");\n")
      | _ :: tl -> helper tl acc
    in

    commonCLibrariesToString commonCLibraries ^ helper main_tree ""
end
