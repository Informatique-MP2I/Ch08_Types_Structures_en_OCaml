let wave_length c =
  match c with
  | Red    -> (620, 750)
  | Green  -> (495,570)
  | Blue   -> (450,495)
  | _      -> (0,0)
