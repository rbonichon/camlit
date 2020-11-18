let () =
  let files = Cli.parse () in
  Format.printf "Ignore %d file names" (List.length files)
