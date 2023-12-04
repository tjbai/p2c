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

  let expr1 = [ "("; "("; "a"; "-"; "b"; ")"; "+"; "c"; ")" ] in
  let expr2 = [ "("; "x"; "/"; "("; "y"; "*"; "z"; ")"; ")" ] in

  assert_equal expr1 @@ split_and_process "((a-b)+c)";
  assert_equal expr2 @@ split_and_process "(x/(y*z))";
  assert_equal (expr1 @ ("-" :: expr2))
  @@ split_and_process "((a-b)+c) - (x/(y*z))"

let test_tokenize_line _ =
  assert_equal
    [ Value "x"; Colon; IntDef; Assign; Value "20"; Newline ]
    ("x: int = 20" |> split_and_process |> tokenize_line);

  assert_equal
    [ While; Value "i"; Bop "<"; Value "10"; Colon; Newline ]
    ("while i < 10:" |> split_and_process |> tokenize_line);

  assert_equal
    [ If; Value "a"; Bop "=="; Value "b"; Colon; Newline ]
    ("if a == b:" |> split_and_process |> tokenize_line);

  assert_equal
    [
      For;
      Value "i";
      In;
      Value "range";
      Lparen;
      Value "10";
      Rparen;
      Colon;
      Newline;
    ]
    ("for i in range(10):" |> split_and_process |> tokenize_line);

  assert_equal
    [
      Lparen;
      Lparen;
      Value "a";
      Bop "-";
      Value "b";
      Rparen;
      Bop "+";
      Value "c";
      Rparen;
      Newline;
    ]
    ("((a - b) + c)" |> split_and_process |> tokenize_line);

  assert_equal
    [
      Lparen;
      Lparen;
      Value "a";
      Bop "-";
      Value "b";
      Rparen;
      Bop "+";
      Value "c";
      Rparen;
      Bop "-";
      Value "x";
      Newline;
    ]
    ("((a-b)+c) - x" |> split_and_process |> tokenize_line);

  assert_equal
    [
      FunDef;
      Value "foo";
      Lparen;
      Value "a";
      Colon;
      IntDef;
      Comma;
      Value "b";
      Colon;
      IntDef;
      Rparen;
      Arrow;
      IntDef;
      Colon;
      Newline;
    ]
    ("def foo(a: int, b: int) -> int:" |> split_and_process |> tokenize_line)

let test_tokenize _ =
  assert_equal
    [
      If;
      Value "a";
      Bop "<";
      Value "b";
      Colon;
      Newline;
      Indent;
      Value "print";
      Lparen;
      Value "a";
      Rparen;
      Newline;
      Dedent;
      Else;
      Colon;
      Newline;
      Indent;
      Value "print";
      Lparen;
      Value "b";
      Rparen;
      Newline;
      Dedent;
    ]
    ("if a < b:\n\tprint(a)\nelse:\n\tprint(b)\n" |> tokenize);

  assert_equal
    [
      FunDef;
      Value "add";
      Lparen;
      Value "a";
      Colon;
      IntDef;
      Comma;
      Value "b";
      Colon;
      IntDef;
      Rparen;
      Arrow;
      IntDef;
      Colon;
      Newline;
      Indent;
      Return;
      Value "a";
      Bop "+";
      Value "b";
      Newline;
      Dedent;
      Newline;
      Value "add";
      Lparen;
      Value "2";
      Bop "+";
      Value "3";
      Rparen;
      Newline;
    ]
    ("def add(a: int, b: int) -> int:\n\treturn a + b \n\nadd(2+3) " |> tokenize)

let tests =
  "Lex tests"
  >: test_list
       [
         "Strip indent" >:: test_strip_indent;
         "Split and process" >:: test_split_and_process;
         "Tokenize line" >:: test_tokenize_line;
         "Tokenize file" >:: test_tokenize;
       ]

let () = run_test_tt_main tests
