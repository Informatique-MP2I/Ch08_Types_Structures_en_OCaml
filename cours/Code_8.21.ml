let read_file filename =
  try
    let channel = open_in filename in
    try
      while true do
        let line = input_line channel in
        Printf.printf "%s\n" line
      done
    with
    | End_of_file -> close_in channel
  with
  | Sys_error err -> Printf.printf "Error : %s\n" err
      
let () = 
  read_file "./VanGogh.md"
