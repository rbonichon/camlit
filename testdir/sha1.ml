let f s =
  let ic = Unix.open_process_in (Printf.sprintf "echo %s | sha1sum" (Str.quote s)) in 
  let s =
    let b = Buffer.create 1024 in 
    let rec loop () =
      match input_line ic with
      | exception End_of_file -> Buffer.contents b
      | l -> Buffer.add_string b l; loop ()
    in loop () in
  let s =
    match String.split_on_char ' ' s with
    | h :: _ -> h
    | [] -> "nothing" in 
  Format.printf "sha1 is %s@." s;
  close_in ic;
;;

let contents file = 
  let ic = open_in_bin file in
  let len = in_channel_length ic in
  let s = really_input_string ic len in 
  close_in ic;
  s
;;

let g file =
  f (contents file)
