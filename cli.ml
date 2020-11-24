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

  let create ?(args = []) ?description ~name action =
    add
      {
        args;
        name;
        description = Option.value description ~default:name;
        action;
      }
end

let umsg = "Too bad"

let with_oid ?err_msg k args =
  let _ = err_msg in 
    let oid =
      match args with
      | oid :: _ -> Base.get_oid oid
      | [] -> Option.get (Data.get_head ())
    in
    k oid

let () =
  let subcommands =
    let open Subcommand in
    create ~name:"init" ~description:"Initialize empty repository" (fun _l ->
        Cmds.init ());
    create ~name:"hash-object" (function
      | file :: _ -> Cmds.hash_file file
      | [] -> assert false);
    [
      {
        name = "cat-file";
        description = "Cat the file designated by the given oid";
        args = [];
        action = with_oid Cmds.cat_file;
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
            match l with
            | oid :: _ -> Base.read_tree (Base.get_oid oid)
            | [] -> assert false);
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
                 Format.printf "%a@." Hash.pp oid);
       });
      {
        name = "log";
        description = "Log commits";
        args = [];
        action = with_oid Cmds.log;
      };
      {
        name = "checkout";
        description = "Checkout given commit id";
        args = [];
        action = with_oid Cmds.checkout;
      };
      {
        name = "tag";
        description = "Tag a specific commit";
        args = [];
        action =
          (function
          | [ tagname ] -> Base.tag_oid tagname (Option.get @@ Data.get_head ())
          | [ oid; tagname ] -> Base.tag_oid tagname (Base.get_oid oid)
          | _ -> assert false);
      };
    ]
  in
  List.iter Subcommand.add subcommands;
  Subcommand.create ~name:"show" ~description:"Show the refs" (with_oid Cmds.show);
  Subcommand.create ~name:"branch" ~description:"Create a new branch"
    (function
     | [name] -> Base.create_branch name (Option.get @@ Data.get_head ())
     | [name; oid] -> Base.create_branch name (Base.get_oid oid)
     | _ -> failwith "usage: branch name [oid]"
    )


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
