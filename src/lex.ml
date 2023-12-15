open Core

[@@@warning "-27"]
[@@@warning "-32"]

type token =
  | Value of string
  | Bop of string
  | Uop of string
  | Assign
  | FunDef
  | Arrow
  | Return
  | IntDef
  | StringDef
  | BoolDef
  | For
  | In
  | While
  | If
  | Elif
  | Else
  | Break
  | Continue
  | Lparen
  | Rparen
  | Comma
  | Colon
  | Indent
  | Dedent
  | Newline
  | Hash
[@@deriving equal, sexp]

(* Constants *)
let whitespace = [ ' '; '\t'; '\n' ]

let binary_ops =
  [ "+"; "*"; "/"; "-"; "and"; "or"; "=="; "!="; "<"; "<="; ">"; ">="; "%" ]

let unary_ops = [ "not" ]

(* Remove first n characters from string *)
let slice_front (s : string) (n : int) : string =
  String.sub s ~pos:n ~len:(String.length s - n)

(* Remove last n characters from string *)
let _slice_back (s : string) (n : int) : string =
  String.sub s ~pos:0 ~len:(String.length s - n)

(* Slice between l and r *)
let slice (s : string) (l : int) (r : int) : string =
  String.sub s ~pos:l ~len:(r - l)

(* Count and remove leading tabs *)
let strip_indent (s : string) : string * int =
  let rec aux (rem : string) (tabs : int) (spaces : int) =
    if String.length rem = 0 then (rem, tabs)
    else
      match rem.[0] with
      | '\t' -> aux (slice_front rem 1) (tabs + 1) spaces
      | ' ' when spaces = 3 -> aux (slice_front rem 1) (tabs + 1) 0
      | ' ' -> aux (slice_front rem 1) tabs (spaces + 1)
      | _ -> (rem, tabs)
  in
  aux s 0 0

(* Split line into whitespace-separated tokens,
   separate values from operators and other semantics *)
let split_and_process (s : string) : string list =
  let seps = [ "("; ")"; ","; ":" ] @ binary_ops in

  let rec close_string (acc : string) (tl : string list) =
    if
      Char.(
        acc.[String.length acc - 1] = '\'' || acc.[String.length acc - 1] = '\"')
    then (acc, tl)
    else
      match tl with
      | s :: tl ->
          let n = String.length s in
          if Char.(s.[n - 1] = '\'' || s.[n - 1] = '\"') then (acc ^ " " ^ s, tl)
          else close_string (acc ^ " " ^ s) tl
      | [] -> failwith "unmatched string in lex"
  in

  let rec separate (s : string) (acc : string list) l r : string list =
    if r = String.length s then slice s l r :: acc |> List.rev
    else if List.mem seps (String.make 1 s.[r]) ~equal:String.( = ) then
      separate s (slice s r (r + 1) :: slice s l r :: acc) (r + 1) (r + 1)
    else separate s acc l (r + 1)
  in

  let rec join (acc : string list) (l : string list) =
    match l with
    | [] -> List.rev acc
    | x :: y :: tl when String.(x = "-" && y = ">") -> join ("->" :: acc) tl
    | hd :: tl when Char.(hd.[0] = '\'' || hd.[0] = '\"') ->
        let string, tl = close_string hd tl in
        join (string :: acc) tl
    | hd :: tl -> join (hd :: acc) tl
  in

  s
  |> String.split_on_chars ~on:whitespace
  |> List.fold ~init:[] ~f:(fun acc el -> acc @ separate el [] 0 0)
  |> List.filter ~f:(fun s -> String.length s > 0)
  |> join []

(* Convert processed line to token types *)
(* NOTE: Maybe  _parse_ some relevant values here? *)
let tokenize_line (line : string list) : token list =
  let rec aux (acc : token list) (comment : bool) (rem : string list) =
    match rem with
    | [] -> List.rev (Newline :: acc)
    | hd :: tl ->
        let next_token =
          match hd with
          | _ when comment -> Value hd
          | "=" -> Assign
          | "def" -> FunDef
          | "->" -> Arrow
          | "return" -> Return
          | "int" -> IntDef
          | "string" -> StringDef
          | "bool" -> BoolDef
          | "for" -> For
          | "in" -> In
          | "while" -> While
          | "if" -> If
          | "elif" -> Elif
          | "else" -> Else
          | "break" -> Break
          | "continue" -> Continue
          | "(" -> Lparen
          | ")" -> Rparen
          | "," -> Comma
          | ":" -> Colon
          | "#" -> Hash
          | _ when List.mem binary_ops hd ~equal:String.( = ) -> Bop hd
          | _ when List.mem unary_ops hd ~equal:String.( = ) -> Uop hd
          | _ -> Value hd (* Either identifier or primitive *)
        in

        aux (next_token :: acc) (comment || equal_token next_token Hash) tl
  in

  let rec join_uops (acc : token list) (ts : token list) =
    match ts with
    | [] -> List.rev acc
    | ((Value _ | Rparen) as hd) :: Bop "-" :: tl ->
        join_uops (Bop "-" :: hd :: acc) tl
    | hd :: Bop "-" :: tl -> join_uops (Uop "-" :: hd :: acc) tl
    | hd :: tl -> join_uops (hd :: acc) tl
  in

  line |> aux [] false |> join_uops []

let repeat (t : token) (n : int) : token list = List.init n ~f:(fun _ -> t)

let tokenize (file : string) : token list =
  let init = ([], 0) in

  let f (tokens, prev_indent) line =
    let line, cur_indent = strip_indent line in

    (* Propagate indent context if line is empty *)
    if String.length line = 0 then (tokens @ [ Newline ], prev_indent)
    else
      let new_tokens = line |> split_and_process |> tokenize_line in
      match cur_indent - prev_indent with
      | c when c > 0 -> (tokens @ repeat Indent c @ new_tokens, cur_indent)
      | c when c < 0 -> (tokens @ repeat Dedent (-c) @ new_tokens, cur_indent)
      | _ -> (tokens @ new_tokens, cur_indent)
  in

  match file |> String.split_lines |> List.fold ~init ~f with
  | t, n -> t @ repeat Dedent n

let show_token (t : token) : string =
  t |> sexp_of_token |> Sexplib.Sexp.to_string_hum

let show_tokens (ts : token list) : string = ts |> List.to_string ~f:show_token
