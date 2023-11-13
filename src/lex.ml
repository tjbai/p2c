open Core

type token =
  | Identifier of string
  | Primitive of string
  | BinaryOp of string
  | Lparen
  | Rparen
  | Comma
  | Colon
  | Indent
  | Dedent
  | Newline
  | Arrow
  | EOF

(* helpers *)
let stripIndent (s : string) : string * int = ("", 0)
let in_range (s : string) (i : int) = String.length s > i

(*
1. Go one line at a time
2. Skip empty lines
3. Strip indents and track indent level
   *)
let tokenize (lines : string list) : token list =
  let rec tokenizeLine (line : string) (acc : token list) : token list = [] in

  (* track tokens and prevIndent as context *)
  let init : token list * int = ([], 0) in

  let f (tokens, prevIndent) line =
    let rem, curIndent = stripIndent line in
    let newTokens = tokenizeLine rem [] in

    if curIndent > prevIndent then (newTokens @ (Indent :: tokens), curIndent)
    else if curIndent < prevIndent then
      (newTokens @ (Dedent :: tokens), curIndent)
    else (newTokens @ tokens, curIndent)
  in

  match List.fold lines ~init ~f with tokens, _ -> List.rev tokens

(* Advance to next non-whitespace character *)
let seek (line : string) : string =
  let is_whitespace (c : char) : bool = Char.( = ) c ' ' || Char.( = ) c '\t' in

  let rec aux (line : string) : string =
    match line.[0] with
    | c when is_whitespace c ->
        String.sub line ~pos:1 ~len:(String.length line - 1)
    | _ -> line
  in

  aux line

(* Eat the next token off the front of a line *)
let chomp (line : string) : string * token = (line, Newline)
