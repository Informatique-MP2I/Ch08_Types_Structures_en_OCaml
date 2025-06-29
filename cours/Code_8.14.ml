(* The description of a square will never be reached. *)
let describe_shape form =
  match form with
  | Circle _ -> "I am a circle."
  | Rectangle _ -> "I am a rectangle."
  | Rectangle (w, h) when w = h -> "I am a square."
  | OtherShape -> raise (Failure "Unrecognized shape.")
