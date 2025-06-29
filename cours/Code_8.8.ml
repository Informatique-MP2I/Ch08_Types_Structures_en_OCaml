let wave_length c = 
  match c with
  | Black  -> (0,0)
  | Red    -> (620,750)
  | Green  -> (495,570)
  | Blue   -> (450,495)
  | White  -> (0,0)

let (wave_length_min,wave_length_max) = wave_length Green 
