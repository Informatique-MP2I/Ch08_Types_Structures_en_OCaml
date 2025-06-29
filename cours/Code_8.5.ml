type time = {
  mutable hours : int;
  minutes       : int;
  seconds       : float
}
let add_three_hours t =
  t.hours <- (t.hours + 3) mod 24
let () =
  let my_time = {hours = 3; minutes = 30; seconds = 0.023} in
  add_three_hours my_time;
  Printf.printf "time : %d h %d min %f s.\n" my_time.hours my_time.minutes my_time.seconds
  

