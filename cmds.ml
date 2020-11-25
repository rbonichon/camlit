open File

let create_directory ?(warn = false) directory_name =
  if warn && Sys.file_exists directory_name then
    Format.eprintf "A directory or file named %s already exists@."
      directory_name;
  Unix.mkdir directory_name 0o700

let init () =
  Format.printf "Initializing directory %s@." default_directory;
  create_directory default_directory;
  create_directory objects_directory

let hash_file file =
  File.read file |> Data.hash_string |> fun (Oid x) ->
  Format.printf "%a@." Hash.pp x

let cat_file oid =
  Data.get_object oid |> Objects.contents |> Format.printf "%s@."

(* Add n whitespaces after each new lines to string s*)
let wrap n ppf s =
  let len = String.length s in
  let blanks = String.make n ' ' in
  let rec loop indent idx =
    if idx < len then (
      if indent then Format.fprintf ppf "%s" blanks;
      let c = s.[idx] in
      Format.pp_print_char ppf c;
      loop (c = '\n') (idx + 1) )
  in
  loop true 0

let log_oid : Format.formatter -> Reference.oid Reference.t -> unit =
 fun ppf (Reference.Direct (Oid h as oid)) ->
  let cmt = Data.get_commit oid in
  Format.fprintf ppf "commit %a@,%a@," Hash.pp h (wrap 4) cmt.message

let log oid =
  let (oids : Reference.oid Reference.t list) = Data.predecessors [ oid ] in
  Format.printf "@[<v 0>%a@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut log_oid)
    oids

let checkout = Base.checkout

let show_oid :
    Format.formatter -> Reference.refname * Reference.oid Reference.t -> unit =
 fun ppf (Refname refname, Direct (Oid oid)) ->
  Format.fprintf ppf "@[<h>%s %a@]@," (refname :> string) Hash.pp (oid :> Hash.t)

let just_oids : 'a * Reference.oid Reference.t -> Reference.oid = function
  | _, Reference.Direct oid -> oid

let show _oid =
  let open Format in
  let refs = Data.get_refs () in
  let ppf = std_formatter in
  pp_open_vbox ppf 0;
  List.iter (show_oid ppf) refs;
  pp_close_box ppf ();
  pp_print_flush ppf ();

  pp_open_vbox ppf 0;
  let oid_set = Data.predecessors (List.map just_oids refs) in
  List.iter
    (function
      | Reference.Direct (Oid h as oid) ->
          let commit = Data.get_commit oid in
          Format.printf "@[<v 2>%a%a@]@," Hash.pp h
            (fun ppf -> function None -> ()
              | Some oid -> Format.fprintf ppf "@,Parent: %a" Hash.pp oid)
            commit.parent)
    oid_set;
  pp_close_box ppf ();
  pp_print_flush ppf ()
