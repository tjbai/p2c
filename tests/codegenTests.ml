open Ast
open Codegen
open OUnit2

(***************************** Expression tests **************************************)
let additionOnly =
  [
    Ast.Expression (Ast.Assignment {name = "a"; value = IntLiteral 5; t = Ast.Int; operator = None});
    Ast.Expression (Ast.Assignment {name = "b"; value = IntLiteral 5; t = Ast.Int; operator = None});
    Ast.Expression (Ast.Assignment {name = "c"; value = IntLiteral 5; t = Ast.Int; operator = None});
    Ast.Expression (Ast.Assignment {name = "d"; value = IntLiteral 5; t = Ast.Int; operator = None});
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

  let addMult_1 =
    [
      Ast.Expression(Assignment {name = "a"; value = IntLiteral 5; t = Ast.Int; operator = None});
      Ast.Expression(Assignment {name = "b"; value = IntLiteral 5; t = Ast.Int; operator = None});
      Ast.Expression(Assignment {name = "c"; value = IntLiteral 5; t = Ast.Int; operator = None});
      Ast.Expression(Assignment {name = "d"; value = IntLiteral 5; t = Ast.Int; operator = None});
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


let mult_1 =
  [
    Ast.Expression(Assignment {name = "a"; value = IntLiteral 5; t = Ast.Int; operator = None});
    Ast.Expression(Assignment {name = "b"; value = IntLiteral 5; t = Ast.Int; operator = None});
    Ast.Expression
      (BinaryOp
         {
           operator = Ast.Multiply;
           left = Identifier "a";
           right = Identifier "b";
         });
  ]

  let addMult_2 =
    [
      Ast.Expression(Assignment {name = "a"; value = IntLiteral 5; t = Ast.Int; operator = None});
      Ast.Expression(Assignment {name = "b"; value = IntLiteral 5; t = Ast.Int; operator = None});
      Ast.Expression(Assignment {name = "c"; value = IntLiteral 5; t = Ast.Int; operator = None});
      Ast.Expression(Assignment {name = "d"; value = IntLiteral 5; t = Ast.Int; operator = None});
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
    let addSubMix =
      [
        Ast.Expression(Assignment {name = "a"; value = IntLiteral 5; t = Ast.Int; operator = None});
        Ast.Expression(Assignment {name = "b"; value = IntLiteral 5; t = Ast.Int; operator = None});
        Ast.Expression(Assignment {name = "c"; value = IntLiteral 5; t = Ast.Int; operator = None});
        Ast.Expression(Assignment {name = "d"; value = IntLiteral 5; t = Ast.Int; operator = None});
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
    
      let multDiv =
        [
          Ast.Expression(Assignment {name = "a"; value = IntLiteral 5; t = Ast.Int; operator = None});
          Ast.Expression(Assignment {name = "b"; value = IntLiteral 5; t = Ast.Int; operator = None});
          Ast.Expression(Assignment {name = "c"; value = IntLiteral 5; t = Ast.Int; operator = None});
          Ast.Expression(Assignment {name = "d"; value = IntLiteral 5; t = Ast.Int; operator = None});
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
    Ast.Expression(Assignment {name = "a"; value = IntLiteral 5; t = Ast.Int; operator = None});
    Ast.Expression(Assignment {name = "b"; value = IntLiteral 5; t = Ast.Int; operator = None});
    Ast.Expression(Assignment {name = "c"; value = IntLiteral 5; t = Ast.Int; operator = None});
    Ast.Expression(Assignment {name = "d"; value = IntLiteral 5; t = Ast.Int; operator = None});
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
    Ast.Expression(Assignment {name = "e"; value = BooleanLiteral true; t = Ast.Boolean; operator = None});
    Ast.Expression(Assignment {name = "f"; value = BooleanLiteral false; t = Ast.Boolean; operator = None});
    Ast.Expression(Assignment {name = "g"; value = BooleanLiteral true; t = Ast.Boolean; operator = None});
    Ast.Expression(Assignment {name = "h"; value = BooleanLiteral false; t = Ast.Boolean; operator = None});
    Ast.Expression
      (BinaryOp
         {
           operator = Ast.And;
           left =
             BinaryOp
               {
                 operator = Ast.Or;
                 left = Identifier "e";
                 right = Identifier "f";
               };
           right =
             BinaryOp
               {
                 operator = Ast.Or;
                 left = Identifier "g";
                 right = Identifier "h";
               };
         });
  ] 

let expression_1 _ =
  assert_equal "int a = 5;\nint b = 5;\nint c = 5;\nint d = 5;\na + b + c + d;\n" @@ ConModule.convertToString additionOnly;
  assert_equal "a = 5;\nb = 5;\nc = 5;\nd = 5;\n(a + b) * (c + d);\n" @@ ConModule.convertToString addMult_1;
  assert_equal "a = 5;\nb = 5;\na * b;\n" @@ ConModule.convertToString mult_1;
  assert_equal "a = 5;\nb = 5;\nc = 5;\nd = 5;\n(a + b) * b * (c + c * d);\n" @@ ConModule.convertToString addMult_2;
  assert_equal "a = 5;\nb = 5;\nc = 5;\nd = 5;\na + b - c + d;\n" @@ ConModule.convertToString addSubMix;
  assert_equal "a = 5;\nb = 5;\nc = 5;\nd = 5;\na * b / c / d;\n" @@ ConModule.convertToString multDiv;
  assert_equal "a = 5;\nb = 5;\nc = 5;\nd = 5;\na * b / b + c - c / d;\n"
  @@ ConModule.convertToString multDivAddSub;
  assert_equal "bool e = True;\nbool f = False;\nbool g = True;\nbool h = False;\n(e || f) && (g || h);\n"@@ ConModule.convertToString and_or

let multplicationExpression = [ Ast.Expression (BinaryOp { operator = Ast.Multiply; left = IntLiteral 5; right = IntLiteral 5; }) ]
let divisionExpression = [ Ast.Expression (BinaryOp { operator = Ast.Divide; left = IntLiteral 5; right = IntLiteral 5; }) ]
let modExpression = [ Ast.Expression (BinaryOp { operator = Ast.Mod; left = IntLiteral 5; right = IntLiteral 5; }) ]
let greaterThanExpression = [ Ast.Expression (BinaryOp { operator = Ast.Gt; left = IntLiteral 5; right = IntLiteral 5; }) ]
let lessThanExpression = [ Ast.Expression (BinaryOp { operator = Ast.Lt; left = IntLiteral 5; right = IntLiteral 5; }) ]
let greaterThanEqualExpression = [ Ast.Expression (BinaryOp { operator = Ast.Gte; left = IntLiteral 5; right = IntLiteral 5; }) ]
let lessThanEqualExpression = [ Ast.Expression (BinaryOp { operator = Ast.Lte; left = IntLiteral 5; right = IntLiteral 5; }) ]
let orExpression = [ Ast.Expression (BinaryOp { operator = Ast.Or; left = IntLiteral 5; right = IntLiteral 5; }) ]
let andExpression = [ Ast.Expression (BinaryOp { operator = Ast.And; left = IntLiteral 5; right = IntLiteral 5; }) ]


let expression_2 _ =
  assert_equal "5 * 5;\n" @@ ConModule.convertToString multplicationExpression;
  assert_equal "5 / 5;\n" @@ ConModule.convertToString divisionExpression;
  assert_equal "5 % 5;\n" @@ ConModule.convertToString modExpression;
  assert_equal "5 > 5;\n" @@ ConModule.convertToString greaterThanExpression;
  assert_equal "5 < 5;\n" @@ ConModule.convertToString lessThanExpression;
  assert_equal "5 >= 5;\n" @@ ConModule.convertToString greaterThanEqualExpression;
  assert_equal "5 <= 5;\n" @@ ConModule.convertToString lessThanEqualExpression;
  assert_equal "5 || 5;\n" @@ ConModule.convertToString orExpression;
  assert_equal "5 && 5;\n" @@ ConModule.convertToString andExpression





(***************************** Assignment tests **************************************)

(*traditional assignment*)
let assignment_eg_1 =
  [
    Ast.Expression
      (Assignment { name = "a"; value = IntLiteral 5; t = Ast.Int; operator = None });
  ]

  (*assignment twice*)
let assignment_eg_2 =
  [
    Ast.Expression
      (Assignment { name = "b"; value = IntLiteral 5; t = Ast.Int; operator = None });
      Ast.Expression
      (Assignment { name = "b"; value = IntLiteral 10; t = Ast.Int; operator = None });
  ]

(*var equal to addition of two *)
let assignment_eg_3 =
  [
    Ast.Expression
      (Assignment
         {
           name = "c";
           value =
             BinaryOp
               {
                 operator = Ast.Add;
                 left = IntLiteral 5;
                 right = IntLiteral 5;
               };
           t = Ast.Int;
           operator = None
         });
  ]

(*var equal to addition of two vars*)
let assignment_eg_4 =
  [
    Ast.Expression(Assignment { name = "d"; value = IntLiteral 5; t = Ast.Int; operator = None });
    Ast.Expression(Assignment { name = "e"; value = IntLiteral 5; t = Ast.Int; operator = None });
    Ast.Expression
      (Assignment
         {
           name = "f";
           value =
             BinaryOp
               {
                 operator = Ast.Add;
                 left = Identifier "d";
                 right = Identifier "e";
               };
           t = Ast.Int;
           operator = None
         });
  ]

(*Need to check assignment out of scope*)
let assignment_eg_5 =
  [
    Ast.Function
      {
        name = "foo";
        parameters = [ ("a", Ast.Int) ];
        return = Ast.Int;
        body =
          [
            Expression(Assignment { name = "c"; value = IntLiteral 5; t = Ast.Int; operator = None });
            Expression(Assignment { name = "d"; value = IntLiteral 5; t = Ast.Int; operator = None });
            Expression
              (BinaryOp
                 {
                   operator = Ast.Add;
                   left = Identifier "c";
                   right = Identifier "d";
                 });
          ];
      };
    Ast.Function
      {
        name = "bar";
        parameters = [ ("a", Ast.Int) ];
        return = Ast.Int;
        body =
          [
            Expression(Assignment { name = "c"; value = IntLiteral 5; t = Ast.Int; operator = None });
            Expression(Assignment { name = "d"; value = IntLiteral 5; t = Ast.Int; operator = None });
            Expression
              (BinaryOp
                 {
                   operator = Ast.Add;
                   left = Identifier "c";
                   right = Identifier "d";
                 });
          ];
      };
  ]
  
let assignment_1 _ =
  assert_equal "int a = 5;\n" @@ ConModule.convertToString assignment_eg_1;
  assert_equal "int b = 5;\nb = 10;\n" @@ ConModule.convertToString assignment_eg_2;
  assert_equal "int c = 5 + 5;\n" @@ ConModule.convertToString assignment_eg_3;
  assert_equal "int d = 5;\nint e = 5;\nint f = d + e;\n" @@ ConModule.convertToString assignment_eg_4;
  assert_equal "int foo(int a){\n\tint c = 5;\n\tint d = 5;\n\tc + d;\n}int bar(int a){\n\tint c = 5;\n\tint d = 5;\n\tc + d;\n}" @@ ConModule.convertToString assignment_eg_5
 

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

  let pass = [ Ast.Pass ]
  let continue = [ Ast.Continue ]
  let break = [ Ast.Break ]

let controlOperators _ =
  assert_equal "return 5;\n" @@ ConModule.convertToString return_eg_1;
  assert_equal "return foo(\"hello\", 5, bar(True, \"Cat\"));\n"
  @@ ConModule.convertToString returnComplex;
  assert_equal "return;\n" @@ ConModule.convertToString pass;
  assert_equal "continue;\n" @@ ConModule.convertToString continue;
  assert_equal "break;\n" @@ ConModule.convertToString break




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

  let coreFuncComplex_str =
    [
      Ast.Function
        {
          name = "bar";
          parameters = [ ("a", Ast.Boolean); ("b", Ast.String) ];
          return = Ast.String;
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
  @@ ConModule.convertToString coreFuncComplex;
  assert_equal
    "string bar(bool astring b){\n\
     \ta + b;\n\
     }printf(%s %d %s , \"hello\", 5, bar(True, \"Cat\"));\n" @@ ConModule.convertToString coreFuncComplex_str

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

(***************************** LOOPS *************************************************)

let forLoop = 
  [
    Ast.For
      {
        value = "i";
        lower = IntLiteral 0;
        upper = IntLiteral 10;
        increment = IntLiteral 1;
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

let whileLoop = 
  [
    Ast.While
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

let loopTests _ = 
  assert_equal "for(int i=0;i<10;i=i+1){\n\ta + b;\n}"
  @@ ConModule.convertToString forLoop;
  assert_equal "while(True){\n\ta + b;\n}" @@ ConModule.convertToString whileLoop

(*************************************************************************************)

let headerTest_1 = [
  Function {
    name = "foo";
    parameters = [("a", Ast.Int)];
    return = Ast.Int;
    body = [
      Expression(Assignment {name = "b"; value = IntLiteral 5; t = Ast.Int; operator = None});
      Expression(Assignment {name = "c"; value = IntLiteral 5; t = Ast.Int; operator = None});
      Expression(BinaryOp {operator = Ast.Add; left = Identifier "b"; right = Identifier "c"});
    ]
  }
]

let test_functionHeaders _ = 
  assert_equal "int foo(int a);\n" @@ GenerateHeader.convertToString headerTest_1

(***************************** UTIL **************************************************)

let codeGenTests =
  "codeGen tests"
  >: test_list
       [
         "assignment tests" >:: assignment_1;
         "return tests" >:: controlOperators;
         "expression_1" >:: expression_1;
          "expression_2" >:: expression_2;
         "function call tests" >:: functionCall_1;
         "core function tests" >:: coreFuncTests;
         "functions tests" >:: functionTests;
         "control tests" >:: testControl;
         "ordering tests" >:: testOrdering;
          "loop tests" >:: loopTests;
          "function headers" >:: test_functionHeaders
       ]

let series = "Final Project Tests" >::: [ codeGenTests ]
let () = run_test_tt_main series
