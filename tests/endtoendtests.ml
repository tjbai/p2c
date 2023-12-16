open OUnit2

let e2e source = source |> Parse.to_ast |> Codegen.ConModule.convertToString

(************** FUNCTION GENERATION TESTS **************)

let withoutMain_eg_py =
  "def sampleFunction(a: int, b: int) -> int:\n\
   \treturn a + b\n\n\
   e = 5\n\
   f = 6\n\
   e + f"

let withoutMain_eg_c =
  "int sampleFunction(int a, int b){\n\
   \treturn a + b;\n\
   }\n\
   int main(){\n\
   \tint e = 5;\n\
   \tint f = 6;\n\
   \te + f;\n\
   }\n"

let testFunctions _ = assert_equal withoutMain_eg_c @@ (withoutMain_eg_py |> e2e)

(**************** CONDITIONAL TESTS **************************)

let if_eg_py =
  "def sampleFunction(a: int, b: int) -> int:\n\
   \tif a > b:\n\
   \t\treturn a\n\
   \telse:\n\
   \t\treturn b"

let if_eg_c =
  "int sampleFunction(int a, int b){\n\
   \tif(a > b){\n\
   \t\treturn a;\n\
   \t}else{\n\
   \t\treturn b;\n\
   \t}\n\
   }\n"

let if_elif_eg_py =
  "def sampleFunction(a: int, b: int) -> int:\n\
   \tif a > b:\n\
   \t\treturn a\n\
   \telif a < b:\n\
   \t\treturn b\n\
   \telse:\n\
   \t\treturn a + b"

let if_elif_eg_c =
  "int sampleFunction(int a, int b){\n\
   \tif(a > b){\n\
   \t\treturn a;\n\
   \t}\telse if(a < b){\n\
   \t\treturn b;\n\
   \t}\n\
   else{\n\
   \t\treturn a + b;\n\
   \t}\n\
   }\n"

let testIf _ =
  assert_equal if_eg_c @@ (if_eg_py |> e2e);

  assert_equal if_elif_eg_c @@ (if_elif_eg_py |> e2e)

(****************************** FUNCTION CALLS TESTS ******************************)
let funcCall_py =
  "def sampleFunction(a: int, b: int) -> int:\n\
   \twhile a > b:\n\
   \t\ta -= 1\n\
   \treturn a\n\n\
   def callFunction():\n\
   \treturn sampleFunction(10, 5)\n\n\
   callFunction()"

let funcCall_c =
  "int sampleFunction(int a, int b){\n\
   \twhile(a > b){\n\
   \t\t a -= 1;\n\
   \t}\n\
   \treturn a;\n\
   }\n\
  \ callFunction(){\n\
   \treturn sampleFunction(10, 5);\n\
   }\n\
   int main(){\n\
   \tcallFunction();\n\
   }\n"

let testFuncCall _ = assert_equal funcCall_c @@ (funcCall_py |> e2e)

(********************** FUNCTIONS WITH COMMENTS TESTS **************************)

let funcWithComments_py =
  "def sampleFunction(a: int, b: int) -> int:\n\
   \t# HELLO! THIS IS A COMMENT!\n\
   \tif a > b:\n\
   \t\treturn a\n\
   \telse:\n\
   \t\treturn b"

let funcWithComments_c =
  "int sampleFunction(int a, int b){\n\
   \t//HELLO! THIS IS A COMMENT!\n\
   \tif(a > b){\n\
   \t\treturn a;\n\
   \t}else{\n\
   \t\treturn b;\n\
   \t}\n\
   }\n"

let testFuncWithComments _ =
  assert_equal funcWithComments_c @@ (funcWithComments_py |> e2e)

(************************** TYPE INFERENCING TESTS **************************)
let inferencing_1 =
  "def sampleFunction(a: int, b: int) -> int:\n\
   \t# HELLO! THIS IS A COMMENT!\n\
   \tc = a + b\n\
   \treturn c"

let inferencing_1_c =
  "int sampleFunction(int a, int b){\n\
   \t//HELLO! THIS IS A COMMENT!\n\
   \tint c = a + b;\n\
   \treturn c;\n\
   }\n"

let inferencing_2 =
  "def foo(a: int, b: int) -> int:\n\
   \treturn a+b\n\n\
   def test(c: int) -> int:\n\
   \treturn c\n\n\
   i = foo(2,3) + test(4)"

let inferencing_2_c =
  "int foo(int a, int b){\n\
   \treturn a + b;\n\
   }\n\
   int test(int c){\n\
   \treturn c;\n\
   }\n\
   int main(){\n\
   \tint i = foo(2, 3) + test(4);\n\
   }\n"

let testTypeInferencing _ =
  assert_equal inferencing_1_c @@ (inferencing_1 |> e2e);

  assert_equal inferencing_2_c @@ (inferencing_2 |> e2e)

let test_issues _ =
  (* https://github.com/tjbai/p2c/issues/23 *)
  assert_equal "i += 1;" @@ e2e "i += 1";
  assert_equal "int i = 0;\ni += 1;" @@ e2e "i = 0\ni += 1";

  (* https://github.com/tjbai/p2c/issues/21 *)
  assert_equal "void foo(){\n\tprintf(\"%s\", hello)\n}"
  @@ e2e "def foo():\n\tprint(\'hello\')";

  (* https://github.com/tjbai/p2c/issues/22 *)
  assert_equal
    "void foo(){\n\
     \tint i = 0;\n\
     \tfor(int x=0;x<=10;x+=1){\n\
     \t\ti += x;\n\
     \t}\n\
     }\n\
     int main(){\n\
     \tint i = 0;\n\
     \tprintf(\"%d\", i);\n\
     }"
  @@ e2e
       "def foo():\n\
        \ti = 0\n\
        \tfor x in range(10):\n\
        \t\ti += x\n\n\
        i = 0\n\
        print(i)"

(* time permitting, the operator precedence feature would also be nice *)

(**************************** TESTS **************************)
let tests =
  "End to End Tests"
  >: test_list
       [
         "testFunctions" >:: testFunctions;
         "testIf" >:: testIf;
         "testFuncCall" >:: testFuncCall;
         "testFuncWithComments" >:: testFuncWithComments;
         "testTypeInferencing" >:: testTypeInferencing;
         "Issues" >:: test_issues;
       ]

let () = run_test_tt_main tests
