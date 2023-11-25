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

(* helpers *)
let _in_range (s : string) (i : int) = String.length s > i

(* Whitespace criterion *)
let is_whitespace (c : char) : bool =
  Char.( = ) c ' ' || Char.( = ) c '\t' || Char.( = ) c '\n'

(* Remove first n characters from string *)
let slice_front (s : string) (n : int) =
  String.sub s ~pos:n ~len:(String.length s - n)

(* Count and remove leading tabs, TODO: count spaces as tabs? *)
let strip_indent (s : string) : string * int =
  let rec aux (rem : string) (i : int) =
    if String.length rem > 0 && Char.( = ) rem.[0] '\t' then
      aux (slice_front rem 1) (i + 1)
    else (rem, i)
  in
  aux s 0

let tokenize_line (line : string) : token list =
  let rec aux (rem : string list) (acc : token list) =
    match rem with
    | [] -> List.rev acc
    | hd :: tl ->
        let next_token =
          match hd with
          | "(" -> Lparen
          | ")" -> Rparen
          | "," -> Comma
          | ":" -> Colon
          | "->" -> Arrow
          | _ -> Newline
        in
        aux tl (next_token :: acc)
  in

  (* Convert to list of tokens *)
  aux (String.split_on_chars line ~on:[ ' '; '\t'; '\n' ]) []

(*
1. Go one line at a time
2. Skip empty lines
3. Strip indents and track indent level
   *)

let tokenize (lines : string list) : token list =
  let f (tokens, prev_indent) line =
    let line, cur_indent = strip_indent line in
    let new_tokens = tokenize_line line in

    match compare cur_indent prev_indent with
    | c when c > 0 -> (new_tokens @ (Indent :: tokens), cur_indent)
    | c when c < 0 -> (new_tokens @ (Dedent :: tokens), cur_indent)
    | _ -> (new_tokens @ tokens, cur_indent)
  in

  match List.fold lines ~init:([], 0) ~f with tokens, _ -> List.rev tokens
