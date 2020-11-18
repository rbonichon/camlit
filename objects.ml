module Type = struct
  type t = Blob

  let to_string = function Blob -> "blob"

  let of_string = function
    | "blob" -> Blob
    | s ->
        let msg =
          Printf.sprintf "Name %s does not correspond to a valid object type." s
        in
        failwith msg
end

type t = { typ : Type.t option; contents : string }

let object_type_delimiter = '\x00'

let of_string s =
  match String.index_opt s object_type_delimiter with
  | None -> { typ = None; contents = s }
  | Some pos ->
      {
        typ = Some (String.sub s 0 pos |> Type.of_string);
        contents = String.sub s (pos + 1) (String.length s - pos - 2);
      }

let to_string s =
  let b = Buffer.create 1024 in
  (* write object type *)
  ( match s.typ with
  | None -> ()
  | Some ty ->
      Buffer.add_string b (Type.to_string ty);
      Buffer.add_char b object_type_delimiter );
  Buffer.add_string b s.contents;
  Buffer.contents b

let create ?(typ = Type.Blob) contents = { typ = Some typ; contents }

let hash t = Hash.hash_string @@ to_string t
