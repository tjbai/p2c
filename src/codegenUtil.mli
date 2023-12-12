module FunctionLookUp : sig
    val generateMapOfFunctionSignatures :
      Ast.statement list  ->  (string, Ast.primitive * (string * Ast.primitive) list) Base.Hashtbl.t
  
    val findReturnType : string -> Ast.statement list -> Ast.primitive
  end
  
  module Common : sig
    val declared_variables : (string, bool) Base.Hashtbl.t
  
    val is_variable_declared : string -> bool
  
    val declare_variable : string -> unit
  
    val clearHashTable : unit -> unit
  
    val checkIfSubAdd : Ast.expression -> bool
  
    val convertBoolToString : bool -> string
  
    val primitiveToString : Ast.primitive -> string
  
    val checkIfAndOperatorPresent : Ast.expression -> bool
  
    val checkIfOrOperatorPresent : Ast.expression -> bool
  
    val getReturnType : Ast.statement list -> Ast.expression list -> string
  
    val binaryToString : Ast.binaryOp option -> string

    val convertArgsListString : (string * Ast.primitive) list -> string
  end