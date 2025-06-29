type time = {
  hours   : int;
  minutes : int;
  seconds : float
} 
let () =
  let t1 = {hours = 15; minutes = 54; seconds = 4.6} in 
  Printf.printf "t1: %dh %dmin %fs.\n" 
    t1.hours t1.minutes t1.seconds ;
  let t2 = {minutes = 13; seconds = 56.2; hours = 8} in 
  Printf.printf "t2: %dh %dmin %fs.\n" 
    t2.hours t2.minutes t2.seconds ;
  let t3 = {t1 with hours = 11; seconds = 23.967} in
  Printf.printf "t3: %dh %dmin %fs.\n" 
    t3.hours t3.minutes t3.seconds
