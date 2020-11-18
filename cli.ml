type args = (Arg.key * Arg.spec * Arg.doc) list

module Subcommand = struct
  type t = { name : string; args : args; description : string }

  let subcommands : (string, t) Hashtbl.t = Hashtbl.create 7

  let add scmd = Hashtbl.add subcommands scmd.name scmd

  let find name = Hashtbl.find_opt subcommands name

  let find_args name =
    match find name with None -> [] | Some { args; _ } -> args
end

let umsg = "Too bad"

let parse () =
  let args = ref [] in
  let parsing_state = ref `Subcommand in
  let files = ref [] in
  Arg.parse_dynamic args
    (fun string ->
      match !parsing_state with
      | `Subcommand ->
          args := Subcommand.find_args string;
          parsing_state := `File
      | `File -> files := string :: !files)
    umsg;
  !files
