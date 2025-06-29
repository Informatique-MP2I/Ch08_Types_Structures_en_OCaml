type shape = Circle of float (* radius *)
           | Rectangle of float * float (* (length, width) *)
           | OtherShape
let area form =
  match form with
  | Circle r -> Some (Float.pi *. r *. r)
  | Rectangle (length, width) -> Some (length *. width)
  | OtherShape -> None

