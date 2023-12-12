open OUnit2

(************** FUNCTION GENERATION TESTS **************)

let withoutMain_eg_py = "def sampleFunction(a: int, b: int) -> int:
return a + b


e = 5
f = 6
e + f
"

let withoutMain_eg_c = "#include \".//testPythonFiles/sample1.h\"
int sampleFunction(int a, int b){
	return a + b;
}
int main(){
	int e = 5;
	int f = 6;
	e + f;
}
"

let testFunctions _ =
  assert_equal  withoutMain_eg_c @@ (withoutMain_eg_py |> Parse.to_ast |> Codegen.ConModule.convertToString )


(**************** CONDITIONAL TESTS **************************)

let if_eg_py = "
  def sampleFunction(a: int, b: int) -> int:
    if a > b:
        return a
    else:
        return b
"

let if_eg_c = "#include \".//testPythonFiles/sample1.h\"
  int sampleFunction(int a, int b){
    if(a > b){
      return a;
    }else {
      return b;
    }
  }
  "

let if_elif_eg_py = "def sampleFunction(a: int, b: int) -> int:
  if a > b:
      return a
  elif a < b:
      return b
  else:
      return a + b
"
let if_elif_eg_c = "#include \".//testPythonFiles/sample1.h\"
  int sampleFunction(int a, int b){
    if(a > b){
      return a;
    }	else if(a < b) {
      return b;
    }else{
      return a + b;
    }
  }
  "

let testIf _ =
  assert_equal if_eg_c @@ (if_eg_py |> Parse.to_ast |> Codegen.ConModule.convertToString );
  assert_equal if_elif_eg_c @@ (if_elif_eg_py |> Parse.to_ast |> Codegen.ConModule.convertToString )



    
(**************************** TESTS **************************)
    let tests =
  "Lex tests"
  >: test_list
       [
         "testFunctions" >:: testFunctions;
          "testIf" >:: testIf;

       ]

let () = run_test_tt_main tests