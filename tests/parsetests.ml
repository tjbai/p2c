open OUnit2
open Ast
open Lex
open Parse

(* string -> expression *)
let toe (s : string) : expression =
  match s |> tokenize |> parse_expression with e, _ -> e

(* string -> statement *)
let tos (s : string) : statement =
  match s |> tokenize |> parse_statement with s, _ -> s

let test_convert _ =
  assert_equal (IntLiteral 0) @@ literal "0";
  assert_equal (IntLiteral 100) @@ literal "100";
  assert_equal (StringLiteral "hello") @@ literal "\"hello\"";
  assert_equal (StringLiteral "test") @@ literal "\'test\'";
  assert_equal (BooleanLiteral false) @@ literal "False";
  assert_equal (BooleanLiteral true) @@ literal "True"

let test_finding_closure _ =
  let find_rparen = find_closure ~l:Lparen ~r:Rparen in

  assert_equal
    ([ Value "a"; Bop "+"; Value "b" ], [ Newline ])
    ("a+b)" |> tokenize |> find_rparen);

  assert_equal
    ([ Lparen; Value "a"; Bop "+"; Value "b"; Rparen ], [ Newline ])
    ("(a+b))" |> tokenize |> find_rparen);

  assert_equal
    ( [
        Lparen;
        Value "a";
        Bop "+";
        Lparen;
        Value "b";
        Bop "+";
        Value "c";
        Rparen;
        Rparen;
        Bop "+";
        Lparen;
        Value "d";
        Bop "+";
        Value "e";
        Rparen;
      ],
      [ Bop "+"; Value "f"; Newline ] )
    ("(a+(b+c)) + (d+e)) + f" |> tokenize |> find_rparen)

let test_simple_expressions _ =
  assert_equal (UnaryOp { operator = Not; operand = Identifier "b" })
  @@ toe "not b";

  assert_equal
    (BinaryOp
       { operator = Multiply; left = Identifier "a"; right = Identifier "b" })
  @@ toe "a*b";

  assert_equal
    (BinaryOp { operator = Divide; left = IntLiteral 2; right = IntLiteral 3 })
  @@ toe "2 / 3";

  assert_equal
    (BinaryOp
       {
         operator = And;
         left = BooleanLiteral false;
         right = BooleanLiteral true;
       })
  @@ toe "False and True";

  assert_equal
    (BinaryOp
       {
         operator = Add;
         left = StringLiteral "concatenate";
         right = StringLiteral "this";
       })
  @@ toe "\"concatenate\" + \'this\'";

  assert_equal
    (Assignment
       {
         name = "helloword";
         t = Unknown;
         value =
           BinaryOp
             {
               operator = Add;
               left = StringLiteral "hello";
               right = StringLiteral "world";
             };
       })
  @@ toe "helloword = \"hello\" + \'world\'"

let test_nested_expressions _ =
  assert_equal
    (BinaryOp
       {
         operator = Add;
         left = Identifier "a";
         right =
           BinaryOp
             { operator = Add; left = Identifier "b"; right = Identifier "c" };
       })
  @@ toe "a+(b+c)";

  assert_equal
    (BinaryOp
       {
         operator = Add;
         left =
           BinaryOp
             { operator = Add; left = Identifier "a"; right = Identifier "b" };
         right = Identifier "c";
       })
  @@ toe "(a+b)+c";

  assert_equal
    (BinaryOp { operator = Add; left = Identifier "a"; right = IntLiteral 10 })
  @@ toe "(((a+10)))";

  assert_equal
    (UnaryOp
       {
         operator = Not;
         operand =
           BinaryOp
             { operator = Add; left = Identifier "a"; right = IntLiteral 10 };
       })
  @@ toe "not (((a+10)))"

let test_operator_precedence _ =
  assert_equal
    (BinaryOp
       {
         operator = Equal;
         left =
           BinaryOp
             { operator = Add; left = Identifier "a"; right = Identifier "b" };
         right =
           BinaryOp
             {
               operator = Multiply;
               left = Identifier "c";
               right = Identifier "d";
             };
       })
  @@ toe "a+b == c*d";

  assert_equal
    (BinaryOp
       {
         operator = Subtract;
         left =
           BinaryOp
             {
               operator = Add;
               left = Identifier "a";
               right =
                 BinaryOp
                   {
                     operator = Multiply;
                     left = Identifier "b";
                     right = Identifier "c";
                   };
             };
         right = Identifier "d";
       })
  @@ toe "a+b*c-d";

  assert_equal
    (BinaryOp
       {
         operator = Multiply;
         left =
           BinaryOp
             { operator = Add; left = Identifier "a"; right = Identifier "b" };
         right = Identifier "c";
       })
  @@ toe "(a+b)*c"

let test_function_calls _ =
  assert_equal
    (FunctionCall
       { name = "foo"; arguments = [ Identifier "a"; Identifier "b" ] })
  @@ toe "foo(a,b)";

  assert_equal
    (FunctionCall
       {
         name = "foo";
         arguments =
           [
             BinaryOp
               { operator = Add; left = Identifier "a"; right = Identifier "b" };
             Identifier "c";
           ];
       })
  @@ toe "foo(a+b, c)";

  assert_equal
    (BinaryOp
       {
         operator = Add;
         left = IntLiteral 10;
         right =
           FunctionCall
             {
               name = "foo";
               arguments = [ IntLiteral 2; StringLiteral "test" ];
             };
       })
  @@ toe "10 + foo(2, \'test\')";

  assert_equal
    (FunctionCall
       {
         name = "a";
         arguments =
           [
             FunctionCall
               {
                 name = "b";
                 arguments =
                   [
                     FunctionCall
                       { name = "c"; arguments = [ BooleanLiteral true ] };
                   ];
               };
           ];
       })
  @@ toe "a(b(c(True)))";

  assert_equal
    (CoreFunctionCall { name = Print; arguments = [ Identifier "some_var" ] })
  @@ toe "print(some_var)"

let test_function_defs _ =
  assert_equal
    (Ast.Return
       (BinaryOp
          { operator = Add; left = Identifier "a"; right = Identifier "b" }))
  @@ tos "return a + b";

  assert_equal
    (Function
       {
         name = "foo";
         parameters = [ ("a", Int); ("b", Int) ];
         return = Int;
         body =
           [
             Return
               (BinaryOp
                  {
                    operator = Add;
                    left = Identifier "a";
                    right = Identifier "b";
                  });
           ];
       })
  @@ tos "def foo(a: int, b: int) -> int:\n\treturn a + b";

  assert_equal
    (Function
       {
         name = "foo";
         parameters = [ ("a", Int) ];
         return = Void;
         body =
           [
             Expression
               (CoreFunctionCall
                  { name = Print; arguments = [ Identifier "a" ] });
           ];
       })
  @@ tos "def foo(a: int):\n\tprint(a)";

  assert_equal
    (Function
       {
         name = "foo";
         parameters = [ ("a", Int) ];
         return = Void;
         body =
           [
             Expression
               (CoreFunctionCall
                  { name = Print; arguments = [ Identifier "a" ] });
             Return (Identifier "a");
           ];
       })
  @@ tos "def foo(a: int):\n\tprint(a)\n\treturn a"

let test_for_loops _ =
  assert_equal
    (Ast.For
       {
         value = "i";
         lower = IntLiteral 0;
         upper = IntLiteral 10;
         increment = IntLiteral 1;
         body =
           [
             Expression
               (CoreFunctionCall
                  { name = Print; arguments = [ Identifier "i" ] });
           ];
       })
  @@ tos "for i in range(10):\n\tprint(i)";

  assert_equal
    (Ast.For
       {
         value = "i";
         lower = IntLiteral 1;
         upper = IntLiteral 10;
         increment = IntLiteral 1;
         body =
           [
             Expression
               (CoreFunctionCall
                  { name = Print; arguments = [ Identifier "i" ] });
           ];
       })
  @@ tos "for i in range(1, 10):\n\tprint(i)";

  assert_equal
    (Ast.For
       {
         value = "i";
         lower =
           BinaryOp
             { operator = Add; left = Identifier "a"; right = Identifier "b" };
         upper = IntLiteral 10;
         increment = IntLiteral 2;
         body =
           [
             Expression
               (CoreFunctionCall
                  { name = Print; arguments = [ Identifier "i" ] });
           ];
       })
  @@ tos "for i in range(a+b, 10, 2):\n\tprint(i)"

let tests =
  "Parse tests"
  >: test_list
       [
         "Convert value to literal" >:: test_convert;
         "Find closing right parentheses" >:: test_finding_closure;
         "Simple expressions" >:: test_simple_expressions;
         "Nested expressions" >:: test_nested_expressions;
         "Operator precedence" >:: test_operator_precedence;
         "Function calls" >:: test_function_calls;
         "Function definitions" >:: test_function_defs;
         "For loops" >:: test_for_loops;
       ]

let () = run_test_tt_main tests