let write_file filename content =
  try
    let channel = open_out filename in   
    output_string channel content;
    close_out channel
  with
  | Sys_error err -> Printf.printf "Error : %s\n" err

let () =
  let line = "Let's not forget that the little emotions are the great captains of our lives and we obey them without realizing it. Vincent Van Gogh.\n" in 
  write_file "./WriteFile.md" line






