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

(* pretty colored strings for output :) *)
let colored (color : int) (message : string) : unit =
  Printf.printf "\027[38;5;%dm%s\027[0m" color message

let green = colored 34
let blue = colored 31

let run_ops (listOfFiles : string list) (verbose : bool) =
  let rec helper listOfFiles =
    match listOfFiles with
    | [] -> ()
    | currentFile :: t ->
        (* setup files *)
        let outputFileC = "./" ^ renamePyFileToC currentFile in
        let outputFileH = "./" ^ renamePyFileToH currentFile in
        let includes = "#include \"" ^ outputFileH ^ "\"" in

        (* convert *)
        let file = FileIO.readFile currentFile in
        let ast = file |> Parse.to_ast in
        let src = includes ^ "\n\n" ^ (ast |> ConModule.convertToString) in
        let header = ast |> GenerateHeader.convertToString in

        (* log *)
        if verbose then (
          blue "Python:\n";
          printf "%s\n\n" file;
          blue "AST:\n";
          printf "%s\n\n" (ast |> Ast.showAst);
          blue "C:\n";
          printf "%s\n" src)
        else ();

        (* write *)
        FileIO.writeFile ~output:outputFileC ~input:src;
        FileIO.writeFile ~output:outputFileH ~input:header;

        helper t
  in
  helper listOfFiles

(* bit of a hacky way to make the output
   more REPL friendly *)
let strip_main (s : string) : string =
  String.sub s ~pos:13 ~len:(String.length s - 15)

let rec repl (verbose : bool) () =
  green ">>> ";
  Out_channel.flush stdout;
  match In_channel.input_line In_channel.stdin with
  | None -> printf "Exiting REPL."
  | Some s when String.(s = "exit") -> printf "Exiting REPL"
  | Some s ->
      (try
         let ast = Parse.to_ast s in
         let c = ast |> ConModule.convertToString |> strip_main in
         if verbose then (
           blue "AST:\n";
           printf "%s\n\n" (ast |> Ast.showAst);
           blue "C:\n")
         else ();
         printf "%s\n" c
       with e -> printf "Encountered error %s\n" (Exn.to_string e));
      repl verbose ()

(* command lines *)
let command =
  Command.basic ~summary:"convert.exe --files <file1> <file2>"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open files = anon (sequence ("source files" %: string))
      and verbose = flag "-v" no_arg ~doc:"enable logging" in
      fun () ->
        match List.length files with
        | 0 -> repl verbose ()
        | _ -> run_ops files verbose)

let () = Command_unix.run command
