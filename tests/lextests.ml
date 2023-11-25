open OUnit2
open Lex

let test_strip_indent _ =
  assert_equal ("", 3) @@ strip_indent "\t\t\t";
  assert_equal ("hello", 1) @@ strip_indent "\thello";
  assert_equal ("hello\ttest", 2) @@ strip_indent "\t\thello\ttest"

let tests = "Lex tests" >: test_list [ "Strip indent" >:: test_strip_indent ]
let () = run_test_tt_main tests
