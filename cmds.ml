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
  File.read file |> Data.hash_string |> Format.printf "%a@." Oid.pp

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

let log oid =
  let oids = Data.predecessors [ oid ] in
  Format.printf "@[<v 0>%a@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut (fun ppf oid ->
         let cmt = Data.get_commit oid in
         Format.fprintf ppf "commit %a@,%a@," Oid.pp oid (wrap 4) cmt.message))
    oids

let checkout = Base.checkout

let show _oid =
  let open Format in
  let refs = Data.get_refs () in
  let ppf = std_formatter in
  pp_open_vbox ppf 0;
  List.iter
    (fun (refname, oid) -> fprintf ppf "@[<h>%s %a@]@," refname Oid.pp oid)
    refs;
  pp_close_box ppf ();
  pp_print_flush ppf ();

  pp_open_vbox ppf 0;
  let oid_set = Data.predecessors (List.map snd refs) in
  List.iter
    (fun oid ->
      let commit = Data.get_commit oid in
      Format.printf "@[<v 2>%a%a@]@," Oid.pp oid
        (fun ppf -> function None -> ()
          | Some oid -> Format.fprintf ppf "@,Parent: %a" Oid.pp oid)
        commit.parent)
    oid_set;
  pp_close_box ppf ();
  pp_print_flush ppf ()
