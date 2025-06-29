let wave_length c =
  match c with
  | Black | White -> (0,0)
  | Red    -> (620, 750)
  | Green  -> (495,570)
  | Blue   -> (450,495)
