type day_time = {
  day   : int;
  mount : int;
  year  : int;
  clock : time
}
let () =
  let my_time={hours=3; minutes=30;seconds=0.023} in
  let my_day_time={day=12;mount=12;year=2023;clock=my_time} in
  Printf.printf "day_time : %d/%d/%d - %d h %d min %f s.\n" 
    my_day_time.day my_day_time.mount my_day_time.year 
    my_day_time.clock.hours my_day_time.clock.minutes my_day_time.clock.seconds
