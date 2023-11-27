open Ast
open Codegen
open OUnit2

(*Sample Tree to Test - a+b
  let adding_1 =
    [
      Ast.Expression
        (BinaryOp
           { operator = Add; left = Identifier "a"; right = Identifier "b" });
    ]
*)
(*
   let assignment_eg_2 =
     [
       Ast.Expression ( )
     ] *)

(*Expression tests*)
(* (let expression _ = assert_equal "a+b;" @@ ConModule.convertToString adding_1  *)

(***************************** Assignment tests **************************************)

let assignment_eg_1 =
  [ Ast.Expression (Assignment { name = "a"; value = IntLiteral 5 }) ]

let assignment_1 _ =
  assert_equal "a = 5;" @@ ConModule.convertToString assignment_eg_1

(***************************** Return tests ******************************************)

let return_eg_1 = [ Ast.Return (IntLiteral 5) ]

let return_1 _ =
  assert_equal "return 5;" @@ ConModule.convertToString return_eg_1

(***************************** UTIL **************************************************)

let codeGenTests =
  "codeGen tests"
  >: test_list
       [ "assignment tests" >:: assignment_1; "return tests" >:: return_1 ]

let series = "Final Project Tests" >::: [ codeGenTests ]
let () = run_test_tt_main series
