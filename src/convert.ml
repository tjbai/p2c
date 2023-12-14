(* Compiles to executable, entrypoint for CLI *)

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

let run_ops (listOfFiles : string list) =
  let rec helper listOfFiles =
    match listOfFiles with
    | [] -> ()
    | currentFile :: t ->
        (* setup files *)
        let outputFileC = "./" ^ renamePyFileToC currentFile in
        let outputFileH = "./" ^ renamePyFileToH currentFile in
        let includes = "#include \"" ^ outputFileH ^ "\"" in

        (* convert *)
        let ast = FileIO.readFile currentFile |> Parse.to_ast in
        let src = includes ^ "\n" ^ (ast |> ConModule.convertToString) in
        let header = ast |> GenerateHeader.convertToString in

        (* log *)
        Printf.printf "Python:\n%s\n" (FileIO.readFile currentFile);
        Printf.printf "AST:\n%s\n" (ast |> Ast.showAst);
        Printf.printf "C:\n%s\n" src;

        (* write *)
        FileIO.writeFile ~output:outputFileC ~input:src;
        FileIO.writeFile ~output:outputFileH ~input:header;

        helper t
  in
  helper listOfFiles

(* command lines *)
let command =
  Command.basic ~summary:"convert.exe --files <file1> <file2>"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open files = anon (sequence ("files" %: string)) in
      fun () -> run_ops files)

let () = Command_unix.run command
