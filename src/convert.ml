(* Compiles to executable, entrypoint for CLI *)



open Core


module FileIO = struct
(*reads in the file*)
  let readFile input =
    let file = In_channel.create input in
    let lines = In_channel.input_lines file in
    In_channel.close file;
    List.fold ~init:"" ~f:(fun acc x -> acc ^ x ^ " ") lines 

  (*writes output file*)
  let writeFile ~output:output ~input:input =
    let file = Out_channel.create output in
    Out_channel.output_string file input;
    Out_channel.close file
end

let renamePyFileToC fileName = 
  let fileName = String.split_on_chars fileName ~on:['.'] in
  let fileName = List.hd_exn fileName in
  fileName ^ ".c"


(*reads in the file*)

let run_ops (listOfFiles:string list) =
  let rec helper listOfFiles = 
    match listOfFiles with
    | [] -> ()
    | currentFile::t -> 
      let outputFileC = renamePyFileToC currentFile in
      let content = FileIO.readFile currentFile |> Parse.toast |> Codegen.ConModule.convertToString in 
      FileIO.writeFile ~output:outputFileC ~input:content;
      helper t
  in helper listOfFiles

(* command lines *)
let command =
  Command.basic
    ~summary:
      "convert.exe --files <file1> <file2>"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open
        files = anon (sequence ("files" %: string))
      in
      fun () -> run_ops files)

let () = Command_unix.run command