open OUnit2
open Lex

let test_strip_indent _ =
  assert_equal ("", 3) @@ strip_indent "\t\t\t";
  assert_equal ("hello", 1) @@ strip_indent "\thello";
  assert_equal ("hello\ttest", 2) @@ strip_indent "\t\thello\ttest"

let test_split_and_process _ =
  assert_equal [] @@ split_and_process "";
  assert_equal [] @@ split_and_process "  ";
  assert_equal [] @@ split_and_process "\t\n\t  \t\n\t";
  assert_equal [ "a"; "b"; "c" ] @@ split_and_process "a b c";
  assert_equal [ "a"; "b"; "c" ] @@ split_and_process "a\tb\nc ";
  assert_equal [ "a"; ","; "b"; ","; "c" ] @@ split_and_process "a, b, c ";
  assert_equal [ "a"; ","; "b"; ","; "c" ] @@ split_and_process "a , b , c";
  assert_equal [ "a"; ":"; "b"; ","; "c" ] @@ split_and_process "a: b, c ";
  assert_equal [ "("; "a"; ")" ] @@ split_and_process "(a)";
  assert_equal [ "("; "("; "a"; "-"; "b"; ")"; "+"; "c"; ")" ]
  @@ split_and_process "((a-b)+c)"

let tests =
  "Lex tests"
  >: test_list
       [
         "Strip indent" >:: test_strip_indent;
         "Split and process" >:: test_split_and_process;
       ]

let () = run_test_tt_main tests
