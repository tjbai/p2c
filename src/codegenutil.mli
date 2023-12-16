module FunctionLookUp : sig
  val findReturnType : string -> Ast.statement list -> Ast.primitive
end

module Common : sig
  val declared_variables : (string, bool) Base.Hashtbl.t
  val is_variable_declared : string -> bool
  val declare_variable : string -> unit
  val clear : unit -> unit
  val checkIfSubAdd : Ast.expression -> bool
  val checkAndOr: Ast.expression -> bool
  val convertBoolToString : bool -> string
  val primitiveToString : Ast.primitive -> string
  val checkIfAndOperatorPresent : Ast.expression -> bool
  val checkIfOrOperatorPresent : Ast.expression -> bool
  val getReturnType : Ast.statement list -> Ast.expression list -> string
  val binaryToString : Ast.binaryOp option -> string
  val convertArgsListString : (string * Ast.primitive) list -> string
  val checkIfElseStatementNext : Ast.statement list -> bool
  val primitiveFuncToString: Ast.primitive -> string
end
