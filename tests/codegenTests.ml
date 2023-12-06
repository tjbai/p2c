open Ast
open Codegen
open OUnit2

(***************************** Expression tests **************************************)
let additionOnly =
  [
    Ast.Expression
      (BinaryOp
         {
           operator = Ast.Add;
           left =
             BinaryOp
               {
                 operator = Ast.Add;
                 left = Identifier "a";
                 right = Identifier "b";
               };
           right =
             BinaryOp
               {
                 operator = Ast.Add;
                 left = Identifier "c";
                 right = Identifier "d";
               };
         });
  ]

let addSubMix =
  [
    Ast.Expression
      (BinaryOp
         {
           operator = Ast.Subtract;
           left =
             BinaryOp
               {
                 operator = Ast.Add;
                 left = Identifier "a";
                 right = Identifier "b";
               };
           right =
             BinaryOp
               {
                 operator = Ast.Add;
                 left = Identifier "c";
                 right = Identifier "d";
               };
         });
  ]

let addMult_1 =
  [
    Ast.Expression
      (BinaryOp
         {
           operator = Ast.Multiply;
           left =
             BinaryOp
               {
                 operator = Ast.Add;
                 left = Identifier "a";
                 right = Identifier "b";
               };
           right =
             BinaryOp
               {
                 operator = Ast.Add;
                 left = Identifier "c";
                 right = Identifier "d";
               };
         });
  ]

let addMult_2 =
  [
    Ast.Expression
      (BinaryOp
         {
           operator = Ast.Multiply;
           left =
             BinaryOp
               {
                 operator = Ast.Multiply;
                 left =
                   BinaryOp
                     {
                       operator = Ast.Add;
                       left = Identifier "a";
                       right = Identifier "b";
                     };
                 right = Identifier "b";
               };
           right =
             BinaryOp
               {
                 operator = Ast.Add;
                 left = Identifier "c";
                 right =
                   BinaryOp
                     {
                       operator = Ast.Multiply;
                       left = Identifier "c";
                       right = Identifier "d";
                     };
               };
         });
  ]

let mult_1 =
  [
    Ast.Expression
      (BinaryOp
         {
           operator = Ast.Multiply;
           left = Identifier "a";
           right = Identifier "b";
         });
  ]

let multDiv =
  [
    Ast.Expression
      (BinaryOp
         {
           operator = Ast.Divide;
           left =
             BinaryOp
               {
                 operator = Ast.Multiply;
                 left = Identifier "a";
                 right = Identifier "b";
               };
           right =
             BinaryOp
               {
                 operator = Ast.Divide;
                 left = Identifier "c";
                 right = Identifier "d";
               };
         });
  ]

let multDivAddSub =
  [
    Ast.Expression
      (BinaryOp
         {
           operator = Ast.Add;
           left =
             BinaryOp
               {
                 operator = Ast.Divide;
                 left =
                   BinaryOp
                     {
                       operator = Ast.Multiply;
                       left = Identifier "a";
                       right = Identifier "b";
                     };
                 right = Identifier "b";
               };
           right =
             BinaryOp
               {
                 operator = Ast.Subtract;
                 left = Identifier "c";
                 right =
                   BinaryOp
                     {
                       operator = Ast.Divide;
                       left = Identifier "c";
                       right = Identifier "d";
                     };
               };
         });
  ]

let and_or =
  [
    Ast.Expression
      (BinaryOp
         {
           operator = Ast.And;
           left =
             BinaryOp
               {
                 operator = Ast.Or;
                 left = Identifier "a";
                 right = Identifier "b";
               };
           right =
             BinaryOp
               {
                 operator = Ast.Or;
                 left = Identifier "c";
                 right = Identifier "d";
               };
         });
  ]

let expression_1 _ =
  assert_equal "a + b + c + d;\n" @@ ConModule.convertToString additionOnly;
  assert_equal "(a + b) * (c + d);\n" @@ ConModule.convertToString addMult_1;
  assert_equal "a * b;\n" @@ ConModule.convertToString mult_1;
  assert_equal "(a + b) * b * (c + c * d);\n"
  @@ ConModule.convertToString addMult_2;
  assert_equal "a + b - c + d;\n" @@ ConModule.convertToString addSubMix;
  assert_equal "a * b / c / d;\n" @@ ConModule.convertToString multDiv;
  assert_equal "a * b / b + c - c / d;\n"
  @@ ConModule.convertToString multDivAddSub;
  assert_equal "(a || b) && (c || d);\n" @@ ConModule.convertToString and_or

(***************************** Assignment tests **************************************)

let assignment_eg_1 =
  [
    Ast.Expression
      (Assignment { name = "a"; value = IntLiteral 5; t = Ast.Int; operator = None });
  ]

let assignment_1 _ =
  assert_equal "int a = 5;\n" @@ ConModule.convertToString assignment_eg_1

(***************************** Return tests ******************************************)

let return_eg_1 = [ Ast.Return (IntLiteral 5) ]

let returnComplex =
  [
    Ast.Return
      (FunctionCall
         {
           name = "foo";
           arguments =
             [
               StringLiteral "hello";
               IntLiteral 5;
               FunctionCall
                 {
                   name = "bar";
                   arguments = [ BooleanLiteral true; StringLiteral "Cat" ];
                 };
             ];
         });
  ]

let return_1 _ =
  assert_equal "return 5;\n" @@ ConModule.convertToString return_eg_1;
  assert_equal "return foo(\"hello\", 5, bar(True, \"Cat\"));\n"
  @@ ConModule.convertToString returnComplex

(***************************** Function Call Tests ***********************************)

let functionCall_eg_1 =
  [
    Ast.Expression
      (FunctionCall
         { name = "foo"; arguments = [ StringLiteral "hello"; IntLiteral 5 ] });
  ]

let functionCall_embeddedFunc =
  [
    Ast.Expression
      (FunctionCall
         {
           name = "foo";
           arguments =
             [
               StringLiteral "hello";
               IntLiteral 5;
               FunctionCall
                 {
                   name = "bar";
                   arguments = [ BooleanLiteral true; StringLiteral "Cat" ];
                 };
             ];
         });
  ]

let functionCall_1 _ =
  assert_equal "foo(\"hello\", 5);\n"
  @@ ConModule.convertToString functionCall_eg_1;
  assert_equal "foo(\"hello\", 5, bar(True, \"Cat\"));\n"
  @@ ConModule.convertToString functionCall_embeddedFunc

(***************************** Core Functions      ***********************************)

let coreFunc_1 =
  [
    Ast.Expression
      (CoreFunctionCall
         {
           name = Ast.Print;
           arguments = [ StringLiteral "hello"; IntLiteral 5 ];
         });
  ]

let coreFuncComplex =
  [
    Ast.Function
      {
        name = "bar";
        parameters = [ ("a", Ast.Boolean); ("b", Ast.String) ];
        return = Ast.Int;
        body =
          [
            Expression
              (BinaryOp
                 {
                   operator = Ast.Add;
                   left = Identifier "a";
                   right = Identifier "b";
                 });
          ];
      };
    Ast.Expression
      (CoreFunctionCall
         {
           name = Ast.Print;
           arguments =
             [
               StringLiteral "hello";
               IntLiteral 5;
               FunctionCall
                 {
                   name = "bar";
                   arguments = [ BooleanLiteral true; StringLiteral "Cat" ];
                 };
             ];
         });
  ]

let coreFuncTests _ =
  assert_equal "printf(%s %d , \"hello\", 5);\n"
  @@ ConModule.convertToString coreFunc_1;
  assert_equal
    "int bar(bool astring b){\n\
     \ta + b;\n\
     }printf(%s %d %d , \"hello\", 5, bar(True, \"Cat\"));\n"
  @@ ConModule.convertToString coreFuncComplex

(***************************** FUNCTIONS *********************************************)
let function_1 =
  [
    Ast.Function
      {
        name = "main";
        parameters = [ ("a", Ast.Int) ];
        return = Ast.Int;
        body =
          [
            Expression
              (BinaryOp
                 {
                   operator = Ast.Divide;
                   left = Identifier "c";
                   right = Identifier "d";
                 });
          ];
      };
  ]

let functionTests _ =
  assert_equal "int main(int a){\n\tc / d;\n}"
  @@ ConModule.convertToString function_1

(***************************** CONTROL **********************************************)

let basicIfStatement =
  [
    Ast.If
      {
        test = BooleanLiteral true;
        body =
          [
            Expression
              (BinaryOp
                 {
                   operator = Ast.Add;
                   left = Identifier "a";
                   right = Identifier "b";
                 });
          ];
      };
  ]

let ifElseStatement =
  [
    Ast.If
      {
        test =
          Ast.BinaryOp
            {
              operator = Ast.Equal;
              left = Identifier "a";
              right = Identifier "b";
            };
        body =
          [
            Expression
              (BinaryOp
                 {
                   operator = Ast.Add;
                   left = Identifier "a";
                   right = Identifier "b";
                 });
          ];
      };
    Ast.Else
      {
        body =
          [
            Expression
              (BinaryOp
                 {
                   operator = Ast.Subtract;
                   left = Identifier "a";
                   right = Identifier "b";
                 });
          ];
      };
  ]

let ifElseIfStatement =
  [
    Ast.If
      {
        test = BooleanLiteral true;
        body =
          [
            Expression
              (BinaryOp
                 {
                   operator = Ast.Add;
                   left = Identifier "a";
                   right = Identifier "b";
                 });
          ];
      };
    Ast.Elif
      {
        test = BooleanLiteral false;
        body =
          [
            Expression
              (BinaryOp
                 {
                   operator = Ast.Subtract;
                   left = Identifier "a";
                   right = Identifier "b";
                 });
          ];
      };
  ]

let testControl _ =
  assert_equal "if(True){\n\ta + b;\n}"
  @@ ConModule.convertToString basicIfStatement;
  assert_equal "if(a == b){\n\ta + b;\n}else {\n\ta - b;\n}"
  @@ ConModule.convertToString ifElseStatement;
  assert_equal "if(True){\n\ta + b;\n}else if(False) {\n\ta - b;\n}"
  @@ ConModule.convertToString ifElseIfStatement

(***************************** ORDERING **************************************************)

let orderSpaceTest_1 =
  [
    Ast.Expression
      (BinaryOp
         {
           operator = Ast.Add;
           left =
             BinaryOp
               {
                 operator = Ast.Multiply;
                 left = Identifier "a";
                 right = Identifier "b";
               };
           right =
             BinaryOp
               {
                 operator = Ast.Add;
                 left = Identifier "c";
                 right = Identifier "d";
               };
         });
    Ast.Expression
      (BinaryOp
         {
           operator = Ast.Subtract;
           left =
             BinaryOp
               {
                 operator = Ast.Subtract;
                 left = Identifier "a";
                 right = Identifier "b";
               };
           right =
             BinaryOp
               {
                 operator = Ast.Add;
                 left = Identifier "c";
                 right = Identifier "d";
               };
         });
  ]

let testOrdering _ =
  assert_equal "a * b + c + d;\na - b - c + d;\n"
  @@ ConModule.convertToString orderSpaceTest_1

(***************************** UTIL **************************************************)

let codeGenTests =
  "codeGen tests"
  >: test_list
       [
         "assignment tests" >:: assignment_1;
         "return tests" >:: return_1;
         "expression_1" >:: expression_1;
         "function call tests" >:: functionCall_1;
         "core function tests" >:: coreFuncTests;
         "functions tests" >:: functionTests;
         "control tests" >:: testControl;
         "ordering tests" >:: testOrdering;
       ]

let series = "Final Project Tests" >::: [ codeGenTests ]
let () = run_test_tt_main series
