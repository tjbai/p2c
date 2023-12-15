open Core
open Codegen

module FileIO = struct
  (*reads in the file*)
  let readFile input = In_channel.read_all input

  (*writes output file*)
  let writeFile ~output ~input =
    let file = Out_channel.create output in
    Out_channel.output_string file input;
    Out_channel.close file
end

module Pretty = struct
  (* pretty colored strings for output :) *)
  let colored (color : int) (message : string) : unit =
    Printf.printf "\027[38;5;%dm%s\027[0m" color message

  let green = colored 34
  let blue = colored 31
  let cyan = colored 36
end

(*converts python name to C equivalent for source files*)
let renamePyFileToC fileName =
  (fileName
  |> String.split_on_chars ~on:[ '.' ]
  |> List.filter ~f:(fun s -> String.length s > 0)
  |> List.hd_exn)
  ^ ".c"

(*converts python name to C equivalent for header files*)
let renamePyFileToH fileName =
  (fileName
  |> String.split_on_chars ~on:[ '.' ]
  |> List.filter ~f:(fun s -> String.length s > 0)
  |> List.hd_exn)
  ^ ".h"

let run_ops (listOfFiles : string list) (verbose : bool) =
  let rec helper listOfFiles =
    match listOfFiles with
    | [] -> ()
    | currentFile :: t -> (
        (* setup files *)
        let outputFileC = "./" ^ renamePyFileToC currentFile in
        let outputFileH = "./" ^ renamePyFileToH currentFile in
        let includes = "#include \"" ^ outputFileH ^ "\"" in

        try
          (* convert *)
          let file = FileIO.readFile currentFile in
          let ast = file |> Parse.to_ast in
          let src = includes ^ "\n\n" ^ (ast |> ConModule.convertToString) in
          let header = ast |> GenerateHeader.convertToString in

          (* log *)
          if verbose then (
            Pretty.blue "Python:\n";
            printf "%s\n\n" file;
            Pretty.blue "AST:\n";
            printf "%s\n\n" (ast |> Ast.showAst);
            Pretty.blue "C:\n";
            printf "%s\n" src)
          else ();

          (* write *)
          FileIO.writeFile ~output:outputFileC ~input:src;
          FileIO.writeFile ~output:outputFileH ~input:header;

          helper t
        with Failure s ->
          Pretty.cyan "Encountered failure:\n";
          printf "%s\n" s)
  in
  helper listOfFiles

(* pull out everything in main, makes more sense for REPL *)
let strip_main (ast : Ast.ast) : Ast.ast =
  match ast with
  | Ast.Function { name = "main"; parameters = _; return = _; body } :: [] ->
      body
  | _ -> ast

(* initiate REPL *)
let rec repl (verbose : bool) (acc : string list) =
  if List.length acc = 0 then Pretty.green ">>> " else Pretty.green "... ";
  Out_channel.flush stdout;

  match In_channel.input_line In_channel.stdin with
  | None -> printf "Leaving REPL"
  | Some s when String.(s = "exit") -> printf "Leaving REPL"
  | Some s when String.(s = "") ->
      acc |> List.rev |> String.concat ~sep:"\n" |> process verbose
  | Some s -> repl verbose (s :: acc)

and process (verbose : bool) (input : string) =
  (try
     Codegenutil.Common.clear ();
     let ast = Parse.to_ast input in
     let c = ast |> strip_main |> ConModule.convertToString in
     if verbose then (
       Pretty.blue "AST:\n";
       printf "%s\n\n" (ast |> Ast.showAst);
       Pretty.blue "C:\n")
     else ();
     printf "%s\n" c
   with Failure s ->
     Pretty.cyan "Encountered error:\n";
     printf "%s\n" s);
  repl verbose []

(* command lines *)
let command =
  Command.basic ~summary:"convert.exe --files <file1> <file2>"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open files = anon (sequence ("source files" %: string))
      and verbose = flag "-v" no_arg ~doc:"enable logging" in
      fun () ->
        match List.length files with
        | 0 -> repl verbose []
        | _ -> run_ops files verbose)

let () = Command_unix.run command
