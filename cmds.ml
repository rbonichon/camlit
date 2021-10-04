let init () =
  Base.init ();
  Format.printf "Initialized empty directory %s@."
    (Filename.concat (Sys.getcwd ()) File.default_directory)

let hash_file file =
  File.read file |> Data.hash_string |> Format.printf "%a@." Oid.pp

let cat_file oid =
  Data.get_object oid |> Objects.contents |> Format.printf "%s@."

(* Add n whitespaces after each new lines to string s *)
let wrap n ppf s =
  let len = String.length s in
  let blanks = String.make n ' ' in
  let rec loop indent idx =
    if idx < len then (
      if indent then Format.fprintf ppf "%s" blanks;
      let c = s.[idx] in
      Format.pp_print_char ppf c;
      loop (c = '\n') (idx + 1))
  in
  loop true 0

let pp_refs ppf = function
  | [] -> ()
  | refs ->
      Format.fprintf ppf "({%a})"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
           Refname.pp)
        refs

let get_oid_to_refs_tbl () =
  let refs = Data.get_refs () in
  let h = Hashtbl.create (List.length refs) in
  List.iter (fun (refname, oid) -> Hashtbl.add h oid refname) refs;
  h

let log oid =
  let oids = Data.predecessors [ oid ] in
  let refs_tbl = get_oid_to_refs_tbl () in
  Format.printf "@[<v 0>%a@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut (fun ppf oid ->
         let cmt = Data.get_commit oid in
         let refs = Hashtbl.find_all refs_tbl oid in
         Format.fprintf ppf "commit %a%a@,%a@," Oid.pp oid pp_refs refs (wrap 4)
           cmt.message))
    oids

let checkout = Base.checkout

let show _oid =
  let open Format in
  let refs = Data.get_refs () in
  let ppf = std_formatter in
  pp_open_vbox ppf 0;
  List.iter
    (fun (refname, oid) ->
      fprintf ppf "@[<h>%a %a@]@," Refname.pp refname Oid.pp oid)
    refs;
  pp_close_box ppf ();
  pp_print_flush ppf ();

  pp_open_vbox ppf 0;
  let oid_set = Data.predecessors (List.map snd refs) in
  List.iter
    (fun oid ->
      let commit = Data.get_commit oid in
      Format.printf "@[<v 2>%a%a@]@," Oid.pp oid
        (fun ppf -> function
          | None -> ()
          | Some oid -> Format.fprintf ppf "@,Parent: %a" Oid.pp oid)
        commit.parent)
    oid_set;
  pp_close_box ppf ();
  pp_print_flush ppf ()

let status () =
  match Base.get_branch_name () with
  | Some branch_name -> Format.printf "On branch %s@." branch_name
  | None ->
      let head = Base.get_oid "@" in
      Format.printf "Head detached at %a@." Oid.pp head

let reset oid = Data.set_head (Ref.O oid)
