let add_three_hours {hours = h} =
  (h + 3) mod 24
let () =
  let my_time = {hours = 15; minutes = 54; seconds = 4.6} in
  Printf.printf "time : %d h %d min %f s.\n" 
    (add_three_hours my_time) my_time.minutes my_time.seconds;
  Printf.printf "time : %d h %d min %f s.\n" 
    my_time.hours my_time.minutes my_time.seconds
