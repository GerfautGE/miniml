(* Usage: miniml <file1> <file2> ... *)

let usage_msg = "Usage: miniml <file1> [<files> ...]"

let input_files = ref []
let output_file = ref "a.out"

let speclist = [
  "--help", Arg.Unit (fun () -> print_endline usage_msg; exit 0), "Display this list of options";
]

let anon_fun filename =
  input_files := filename :: !input_files

let open_file (filename: string): string =
  let ic = open_in filename in
  match ic with
  | exception Sys_error msg -> Printf.eprintf "%s\n" msg; exit 1
  | _ -> let n = in_channel_length ic in
         let s = really_input_string ic n in
  close_in ic;
  s
;;

let sources: string list =
  Arg.parse speclist anon_fun usage_msg;
  List.map open_file (List.rev !(input_files))
