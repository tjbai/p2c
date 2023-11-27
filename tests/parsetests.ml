open OUnit2
open Ast
open Lex
open Parse

let test_convert _ =
  assert_equal (IntLiteral 0) @@ convert "0";
  assert_equal (IntLiteral 100) @@ convert "100";
  assert_equal (StringLiteral "hello") @@ convert "\"hello\"";
  assert_equal (StringLiteral "test") @@ convert "\'test\'";
  assert_equal (BooleanLiteral false) @@ convert "False";
  assert_equal (BooleanLiteral true) @@ convert "True"

let test_finding_closure _ =
  assert_equal
    ([ Value "a"; Bop "+"; Value "b" ], [ Newline ])
    ("a+b)" |> tokenize |> find_closure);

  assert_equal
    ([ Lparen; Value "a"; Bop "+"; Value "b"; Rparen ], [ Newline ])
    ("(a+b))" |> tokenize |> find_closure);

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
    ("(a+(b+c)) + (d+e)) + f" |> tokenize |> find_closure)

let test_simple_expressions _ =
  (* string -> expression *)
  let toe (s : string) : expression =
    match s |> tokenize |> parse_expression with e, _ -> e
  in

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

let tests =
  "Parse tests"
  >: test_list
       [
         "Convert value to literal" >:: test_convert;
         "Simple expressions" >:: test_simple_expressions;
         "Find closing right parentheses" >:: test_finding_closure;
       ]

let () = run_test_tt_main tests
