type contents = string

type commit = { oid : Oid.t; parent : Oid.t option; message : string }

type _ t = Tree : 'a -> 'a t | Blob : 'a -> 'a t | Commit : commit -> 'a t

let object_type_delimiter = '\x00'

let tree contents = Tree contents

let blob contents = Blob contents

let commit ?parent oid message = Commit { oid; parent; message }

let contents = function Tree c | Blob c -> c | Commit _ -> assert false

(* let read_commit s =
 *   let coid = ref None in
 *   let parent = ref None in
 *   let b = Buffer.create 1023 in
 *   let lines = String.split_on_char '\n' s in
 *   let rec read_message = function
 *     | [] -> ()
 *     | line :: lines ->
 *         Buffer.add_string b line;
 *         Buffer.add_char b '\n';
 *         read_message lines
 *   in
 * 
 *   let rec read_keys l =
 *     match l with
 *     | [] -> ()
 *     | line :: lines -> (
 *         match String.split_on_char ' ' line with
 *         | [ "tree"; oid ] ->
 *             coid := Some (Oid.of_hex oid);
 *             read_keys lines
 *         | [ "parent"; oid ] ->
 *             parent := Some (Oid.of_hex oid);
 *             read_keys lines
 *         | _ -> read_message l )
 *   in
 * 
 *   read_keys lines;
 *   match !coid with
 *   | None -> failwith "Could not find object id in commit object"
 *   | Some oid ->
 *       let parent = !parent in
 *       commit ?parent oid (Buffer.contents b) *)

let read_key_value ib =
  Scanf.bscanf ib "%[a-z]@ %[a-f0-9]@\n" (fun k v -> (k, v))

let read_key_values ib =
  let rec loop acc =
    Scanf.bscanf ib "%r%0c" read_key_value (fun kv c ->
        let acc = kv :: acc in
        if c = '\n' then acc else loop acc)
  in
  loop []

let read_commit s =
  let ib = Scanf.Scanning.from_string s in
  Scanf.bscanf ib "%[a-z]@\000%r %[\000-\255]" read_key_values
    (fun obj_type kvs msg ->
      assert (obj_type = "commit");
      let find_key_oid ?(on_none = fun () -> None) key =
        match List.assoc_opt key kvs with
        | Some oid -> Some (Oid.of_hex oid)
        | None -> on_none ()
      in
      let commit_tree_id =
        Option.get
        @@ find_key_oid
             ~on_none:(fun () ->
               failwith "Could not find object id in commit object")
             "tree"
      in
      let parent = find_key_oid "parent" in
      commit ?parent commit_tree_id msg)

let of_string s =
  match String.index_opt s object_type_delimiter with
  | None -> Blob s
  | Some pos -> (
      let contents = String.sub s (pos + 1) (String.length s - pos - 1) in
      match String.sub s 0 pos with
      | "blob" -> blob contents
      | "tree" -> tree contents
      | "commit" -> read_commit s
      | s ->
          let msg = Printf.sprintf "Unkown object type %s" s in
          failwith msg )

let type_str t =
  match t with Tree _ -> "tree" | Blob _ -> "blob" | Commit _ -> "commit"

let pp_contents ppf t =
  match t with
  | Tree contents | Blob contents -> Format.pp_print_string ppf contents
  | Commit { oid; parent; message } ->
      Format.fprintf ppf "tree %a@\n" Oid.pp oid;
      ( match parent with
      | Some s -> Format.fprintf ppf "parent %a@\n" Oid.pp s
      | None -> () );
      Format.pp_print_string ppf message

let to_string t =
  Format.asprintf "%s%c%a" (type_str t) object_type_delimiter pp_contents t

let hash t = Oid.of_string @@ to_string t
