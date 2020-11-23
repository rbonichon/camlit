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

  let pp_names ppf () =
    Hashtbl.iter (fun name _ -> Format.fprintf ppf "%s@ " name) subcommands
end

let umsg = "Too bad"

let () =
  let subcommands =
    let open Subcommand in
    [
      {
        name = "init";
        description = "Initialize empty repository";
        args = [];
        action = (fun _l -> Cmds.init ());
      };
      {
        name = "hash-object";
        description = "Hash object";
        args = [];
        action =
          (fun files ->
            match files with
            | file :: _ -> Cmds.hash_file file
            | [] -> assert false);
      };
      {
        name = "cat-file";
        description = "Cat the file designated by the given oid";
        args = [];
        action =
          (fun oids ->
            match oids with oid :: _ -> Cmds.cat_file oid | [] -> assert false);
      };
      {
        name = "write-tree";
        description = "";
        args = [];
        action =
          (fun _ ->
            let oid = Base.write_tree ~directory:"." in
            Format.printf "%a@." Hash.pp oid);
      };
      {
        name = "read-tree";
        description = "";
        args = [];
        action =
          (fun l ->
            match l with oid :: _ -> Base.read_tree oid | [] -> assert false);
      };
      (let msg = ref None in
       {
         name = "commit";
         args =
           [
             ( "-message",
               Arg.String (fun s -> msg := Some s),
               " Commit message (required)" );
           ];
         description = "Save a snaphost of the work tree";
         action =
           (fun _ ->
             match !msg with
             | None -> failwith "Committing requires a non-empty commit message"
             | Some message ->
                let oid = Base.commit ~message in
                Format.printf "%a@." Hash.pp oid 
           );
       }
      );
      { name = "log";
        description = "Log commits";
        args = [];
        action =
          (fun oids ->
            match oids with
            | oid :: _ ->
               let oid = Some (Hash.of_hex oid) in
               Cmds.log ~oid ()
            | [] -> Cmds.log ())
      }
    ]
  in
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
              Format.eprintf
                "@[<v>Unknown subcommand %s@,\
                 Known subcommands are:@,\
                 @[<hov>%a@]@,\
                 @]\n\
                \                " string Subcommand.pp_names ();

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
