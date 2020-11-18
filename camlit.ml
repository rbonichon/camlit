let args = ref [ ("-init", Arg.Unit Cmds.init, "") ]

let subcommands = [ ("init", Arg.Unit Cmds.init) ]

let umsg = "Bad usage. Refer to documentation."

type parsing_state = Subcommand | File

let () =
  let parsing_state = ref Subcommand in
  let files = ref [] in
  Arg.parse_dynamic args
    (fun str ->
      match !parsing_state with
      | File -> files := str :: !files
      | Subcommand ->
          args :=
            [ ("-foo", Arg.Unit (fun () -> print_endline "foo"), " Test") ];
          parsing_state := File)
    umsg;
  Format.printf "Ignore %d file names" (List.length !files)
