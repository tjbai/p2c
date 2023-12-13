open OUnit2

(************** UTILITIES **************)
(* 
  let string_to_list str =
    let rec loop i limit =
      if i = limit then []
      else str.[i] :: loop (i + 1) limit
    in
    loop 0 (String.length str)
  let escape_chars (s : string) : string =
  let escape_char = function
    | '\n' -> "\\n"
    | '\t' -> "\\t"
    | '\\' -> "\\\\"
    | c -> String.make 1 c
  in
  String.concat "" (List.map escape_char (  s |> string_to_list )) *)
  
(************** FUNCTION GENERATION TESTS **************)

let withoutMain_eg_py = "def sampleFunction(a: int, b: int) -> int:\n\treturn a + b\n\ne = 5\nf = 6\ne + f"
let withoutMain_eg_c = "int sampleFunction(int a, int b){\n\treturn a + b;\n}\nint main(){\n\tint e = 5;\n\tint f = 6;\n\te + f;\n}\n"

let testFunctions _ =
    assert_equal  withoutMain_eg_c @@ (withoutMain_eg_py |> Parse.to_ast |> Codegen.ConModule.convertToString )


(**************** CONDITIONAL TESTS **************************)

let if_eg_py = "def sampleFunction(a: int, b: int) -> int:\n\tif a > b:\n\t\treturn a\n\telse:\n\t\treturn b"

let if_eg_c = "int sampleFunction(int a, int b){\n\tif(a > b){\n\t\treturn a;\n\t}else{\n\t\treturn b;\n\t}\n}\n"

let if_elif_eg_py = "def sampleFunction(a: int, b: int) -> int:\n\tif a > b:\n\t\treturn a\n\telif a < b:\n\t\treturn b\n\telse:\n\t\treturn a + b"
let if_elif_eg_c = "int sampleFunction(int a, int b){\n\tif(a > b){\n\t\treturn a;\n\t}\telse if(a < b) {\n\t\treturn b;\n\t}else{\n\t\treturn a + b;\n\t}\n}\n"
let testIf _ =
  assert_equal if_eg_c @@ (if_eg_py |> Parse.to_ast |> Codegen.ConModule.convertToString );
  assert_equal if_elif_eg_c @@ (if_elif_eg_py |> Parse.to_ast |> Codegen.ConModule.convertToString )

(****************************** FUNCTION CALLS TESTS ******************************)
let funcCall_py = "def sampleFunction(a: int, b: int) -> int:\n\twhile a > b:\n\t\ta -= 1\n\treturn a\n\ndef callFunction():\n\treturn sampleFunction(10, 5)\n\ncallFunction()"

let funcCall_c = "int sampleFunction(int a, int b){\n\twhile(a > b){\n\t\t a =- 1;\n\t}\n\treturn a;\n}\n callFunction(){\n\treturn sampleFunction(10, 5);\n}\nint main(){\n\tcallFunction();\n}\n"


let testFuncCall _ =
  assert_equal funcCall_c @@ (funcCall_py |> Parse.to_ast |> Codegen.ConModule.convertToString )

(********************** FUNCTIONS WITH COMMENTS TESTS **************************)



let funcWithComments_py = "def sampleFunction(a: int, b: int) -> int:\n\t# HELLO! THIS IS A COMMENT!\n\tif a > b:\n\t\treturn a\n\telse:\n\t\treturn b"

let funcWithComments_c = "int sampleFunction(int a, int b){\n\t//HELLO! THIS IS A COMMENT!\n\tif(a > b){\n\t\treturn a;\n\t}else{\n\t\treturn b;\n\t}\n}\n"

let testFuncWithComments _ =
  assert_equal funcWithComments_c @@ (funcWithComments_py |> Parse.to_ast |> Codegen.ConModule.convertToString )

    
(**************************** TESTS **************************)
    let tests =
  "End to End Tests"
  >: test_list
       [
         "testFunctions" >:: testFunctions;
          "testIf" >:: testIf;
          "testFuncCall" >:: testFuncCall;
          "testFuncWithComments" >:: testFuncWithComments;
       ]

let () = run_test_tt_main tests