type t = string 

let read file =
  let ic = open_in file in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s
;;

