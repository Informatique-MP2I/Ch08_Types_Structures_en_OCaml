let wave_length c =
  if c = Black then (* Black is not a color. *)
    (0,0) 
  else if c = Red then
    (620,750)
  else if c = Green then
    (495,570)
  else if c = Blue then
    (450,495)
  else if c = White then (* White is a mix of all colors. *)
    (0,0)
  else
    failwith "The given color is not primary."

let (wave_length_min,wave_length_max) = wave_length Green 
