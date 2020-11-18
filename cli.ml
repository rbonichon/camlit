type args = (Arg.key * Arg.spec * Arg.doc) list

type files = string list

module Subcommand = struct
  type t = {
    name : string;
    args : args;
    description : string;
    action : files -> unit;
  }

  let subcommands : (string, t) Hashtbl.t = Hashtbl.create 7

  let add scmd = Hashtbl.add subcommands scmd.name scmd
  
  let find name = Hashtbl.find_opt subcommands name
end

let umsg = "Too bad"

let () =
  let subcommands =
    let open Subcommand in 
    [
      { name = "init";
        description = "Initialize empty repository";
        args = [];
        action = fun _l -> Cmds.init ();
      };
      { name = "hash-object";
      description = "Hash object";
      args = [];
      action = fun files -> match files with file :: _ -> Cmds.hash_object file | [] -> assert false ; }
    ] in 
  List.iter Subcommand.add subcommands


let parse () =
  let args = ref [] in
  let files = ref [] in
  let subcommand = ref None in

  Arg.parse_dynamic args
    (fun string ->
      match !subcommand with
      | None -> (
          match Subcommand.find string with
          | None ->
              Format.eprintf "Unknown subcommand %s@." string;
              exit 1
          | Some scmd as scmd_opt ->
              let open Subcommand in
              args := scmd.args;
              subcommand := scmd_opt )
      | Some _ -> files := string :: !files)
    umsg;
  match !subcommand with
  | None -> Format.printf "No subcommand used. Doing nothing.@."
  | Some scmd -> scmd.action !files
