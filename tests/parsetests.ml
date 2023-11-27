open OUnit2
open Parse
open Lex
open Ast

let test_convert _ =
  assert_equal (IntLiteral 0) @@ convert (Value "0");
  assert_equal (IntLiteral 100) @@ convert (Value "100");
  assert_equal (StringLiteral "hello") @@ convert (Value "\"hello\"");
  assert_equal (StringLiteral "test") @@ convert (Value "\'test\'");
  assert_equal (BooleanLiteral false) @@ convert (Value "False");
  assert_equal (BooleanLiteral true) @@ convert (Value "True")

let tests =
  "Parse tests" >: test_list [ "Convert value to literal" >:: test_convert ]

let () = run_test_tt_main tests
