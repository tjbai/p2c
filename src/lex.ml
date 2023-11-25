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
let inRange (s : string) (i : int) = String.length s > i

(* Remove first n characters from string *)
let sliceFront (s : string) (n : int) =
  String.sub s ~pos:n ~len:(String.length s - n)

(* Advance to next non-whitespace character *)
let seek (line : string) : string =
  let is_whitespace (c : char) : bool = Char.( = ) c ' ' || Char.( = ) c '\t' in

  let rec aux (line : string) : string =
    match line.[0] with
    | c when is_whitespace c -> aux (sliceFront line 1)
    | _ -> line
  in

  aux line

(* Read the next token from the start of a string *)
let chomp (line : string) : string * token = (line, Newline)

(* Remove leading \t, and return how many there are *)
let stripIndent (s : string) : string * int =
  let rec aux (rem : string) (i : int) =
    if String.length rem > 0 && Char.( = ) rem.[0] '\t' then
      aux (seek rem) (i + 1)
    else (rem, i)
  in
  aux s 0

let tokenizeLine (line : string) : token list =
  (* let rec aux (rem : string) (acc : token list) =
       match (rem, acc) with _ -> []
     in
     aux line [] *)
  match line with _ -> []

(*
1. Go one line at a time
2. Skip empty lines
3. Strip indents and track indent level
   *)

let tokenize (lines : string list) : token list =
  let f (tokens, prevIndent) line =
    let line, curIndent = stripIndent line in
    let newTokens = tokenizeLine line in

    match compare curIndent prevIndent with
    | c when c > 0 -> (newTokens @ (Indent :: tokens), curIndent)
    | c when c < 0 -> (newTokens @ (Dedent :: tokens), curIndent)
    | _ -> (newTokens @ tokens, curIndent)
  in

  match List.fold lines ~init:([], 0) ~f with tokens, _ -> List.rev tokens
