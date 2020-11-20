type contents = string

type 'a t =
  | Tree of 'a
  | Blob of 'a
  | Commit of { oid : Hash.t;
                message : string; }

let object_type_delimiter = '\x00'

let tree contents = Tree contents

let blob contents = Blob contents

let commit oid message =
  Commit { oid; message; }



let contents = function Tree c | Blob c -> c | Commit _ -> assert false

let of_string s =
  match String.index_opt s object_type_delimiter with
  | None -> Blob s
  | Some pos ->
      let contents = String.sub s (pos + 1) (String.length s - pos - 2) in
      let typ =
        match String.sub s 0 pos with
        | "blob" -> blob
        | "tree" -> tree
        | s ->
            let msg = Printf.sprintf "Unkown object type %s" s in
            failwith msg
      in
      typ contents

let to_string t =
  match t with
  | (Tree contents | Blob contents) as t' ->
     let b = Buffer.create 1024 in
     (match t' with
      | Commit _ -> assert false
      | Tree _ ->
        Buffer.add_string b "tree"
      | Blob _ ->
         Buffer.add_string b "blob");
     Buffer.add_char b object_type_delimiter;
     Buffer.add_string b contents;
     Buffer.contents b
  | Commit {oid; message;} ->
     Format.asprintf "@[<v>tree %a@,@,%s@]@." Hash.pp oid message

let hash t = Hash.of_string @@ to_string t
