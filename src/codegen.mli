(*CodeGen module*)

module type CodeGen = sig
  val convertToString : Ast.statement list -> string
end

module ConModule : CodeGen

module GenerateHeader: CodeGen