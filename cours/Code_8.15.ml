let describe_shape form =
  match form with
  | Circle _ -> "I am a circle."
  | Rectangle (w, h) when w = h -> "I am a square."
  | Rectangle _ -> "I am a rectangle."
  | OtherShape -> raise (Failure "Unrecognized shape.")
