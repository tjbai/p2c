open OUnit2

(************** FUNCTION GENERATION TESTS **************)

let withoutMain_eg_py =
  "def sampleFunction(a: int, b: int) -> int:\n\
   return a + b\n\n\n\
   e = 5\n\
   f = 6\n\
   e + f\n"

let withoutMain_eg_c =
  "#include \".//testPythonFiles/sample1.h\"\n\
   int sampleFunction(int a, int b){\n\
   \treturn a + b;\n\
   }\n\
   int main(){\n\
   \tint e = 5;\n\
   \tint f = 6;\n\
   \te + f;\n\
   }\n"

let testFunctions _ =
  assert_equal withoutMain_eg_c
  @@ (withoutMain_eg_py |> Parse.to_ast |> Codegen.ConModule.convertToString)

(**************** CONDITIONAL TESTS **************************)

let if_eg_py =
  "def sampleFunction(a: int, b: int) -> int:\n\
  \    if a > b:\n\
  \        return a\n\
  \    else:\n\
  \        return b\n"

let if_eg_c =
  "#include \".//testPythonFiles/sample1.h\"\n\
  \  int sampleFunction(int a, int b){\n\
  \    if(a > b){\n\
  \      return a;\n\
  \    }else {\n\
  \      return b;\n\
  \    }\n\
  \  }\n\
  \  "

let if_elif_eg_py =
  "def sampleFunction(a: int, b: int) -> int:\n\
  \ \tif a > b:\n\
  \      return a\n\
  \  elif a < b:\n\
  \      return b\n\
  \  else:\n\
  \      return a + b\n"

let if_elif_eg_c =
  "#include \".//testPythonFiles/sample1.h\"\n\
  \  int sampleFunction(int a, int b){\n\
  \    if(a > b){\n\
  \      return a;\n\
  \    }\telse if(a < b) {\n\
  \      return b;\n\
  \    }else{\n\
  \      return a + b;\n\
  \    }\n\
  \  }\n\
  \  "

let testIf _ =
  assert_equal if_eg_c
  @@ (if_eg_py |> Parse.to_ast |> Codegen.ConModule.convertToString);
  assert_equal if_elif_eg_c
  @@ (if_elif_eg_py |> Parse.to_ast |> Codegen.ConModule.convertToString)

(****************************** FUNCTION CALLS TESTS ******************************)
let funcCall_py =
  "def sampleFunction(a: int, b: int) -> int:\n\
   \twhile a > b:\n\
   \t\ta -= 1\n\
   \treturn a\n\n\n\
   def callFunction():\n\
   \treturn sampleFunction(10, 5)\n\n\n\
   callFunction()\n"

let funcCall_c =
  "#include \".//testPythonFiles/sample1.h\"\n\
   int sampleFunction(int a, int b){\n\
   \treturn a;\n\
   }\n\
   void callFunction(){\n\
   \treturn sampleFunction(10, 5);\n\
   }\n\
   int main(){\n\
   \tcallFunction();\n\
   }\n"

let testFuncCall _ =
  assert_equal funcCall_c
  @@ (funcCall_py |> Parse.to_ast |> Codegen.ConModule.convertToString)

(**************************** TESTS **************************)
let tests =
  "End to End Tests"
  >: test_list
       [
         "testFunctions" >:: testFunctions;
         "testIf" >:: testIf;
         "testFuncCall" >:: testFuncCall;
       ]

let () = run_test_tt_main tests
