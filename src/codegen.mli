
open Core


module CodeGen(Tree: ast) = sig
  
    type t 

    val convertTreeToText: Tree:ast -> string


end