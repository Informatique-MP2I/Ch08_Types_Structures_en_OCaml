let () =
  let time1 = {hours = 15; minutes = 54; seconds = 4.6} in
  let time2 = {minutes = 13; seconds = 56.2; hours = 8} in
  let h1 = time1.hours in
  let {hours = h2; minutes = min2} = time2 in
  Printf.printf "h1: %d, h2: %d, min2: %d.\n" h1 h2 min2
