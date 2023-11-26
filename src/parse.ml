open Ast
open Lex

(* NOTE: Nead to figure how to thread context along through different calls *)

(* Value s -> IntLiteral | StringLiteral | BooleanLiteral | Identifier *)
let _infer_type (t : token) : expression = match t with _ -> IntLiteral 0

(* Parse a complete expression from tokens *)
let _parse_expression (ts : token list) : expression * token list =
  match ts with _ -> (IntLiteral 0, [])

(* Parse a complete statement from tokens *)
let parse_statement (ts : token list) : statement * token list =
  match ts with _ -> (Break, [])

let parse (tokens : token list) : ast =
  let rec aux (t : token list) (acc : ast) : ast * token list =
    match t with
    (* Out of tokens, return acc *)
    | [] -> (List.rev acc, [])
    (* Skip empty lines *)
    | Newline :: tl -> aux tl acc
    | _ -> (
        match parse_statement t with
        | new_statement, tl -> aux tl (new_statement :: acc))
  in
  match aux tokens [] with ast, _ -> ast
