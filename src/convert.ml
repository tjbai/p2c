(* Compiles to executable, entrypoint for CLI *)

open Core

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
let renamePyFileToC fileName =
  (fileName
  |> String.split_on_chars ~on:[ '.' ]
  |> List.filter ~f:(fun s -> String.length s > 0)
  |> List.hd_exn)
  ^ "."

let run_ops (listOfFiles : string list) =
  let rec helper listOfFiles =
    match listOfFiles with
    | [] -> ()
    | currentFile :: t ->
        let outputFileC = "./"^renamePyFileToC currentFile in
        let outputFileH = "./"^renamePyFileToC currentFile in 
        let includeStatement = "#include \"" ^ outputFileH ^ "\"" in
        let srcContent =
          Stdio.printf "%s\n" (FileIO.readFile currentFile);
          FileIO.readFile currentFile
          |> Parse.to_ast |> Codegen.ConModule.convertToString
        in
        let headerContent = 
          FileIO.readFile currentFile
          |> Parse.to_ast |> Codegen.GenerateHeader.convertToString
        in
        FileIO.writeFile ~output:outputFileC ~input:(includeStatement^"\n"^srcContent);
        FileIO.writeFile ~output:outputFileH ~input:(headerContent);
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
