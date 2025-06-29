let wave_length c =
  match c with (* Incorrect matching *)
  | _      -> (0,0)
  | Red    -> (620, 750)
  | Green  -> (495,570)
  | Blue   -> (450,495)
