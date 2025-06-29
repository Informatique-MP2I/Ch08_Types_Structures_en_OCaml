type shape = Circle of float (* radius *)
           | Rectangle of float * float (* (length, width) *)
           | OtherShape

let area form =
  match form with
  | Circle radius -> Float.pi *. radius *. radius
  | Rectangle (length, width) -> length *. width
  | OtherShape -> raise (Failure "The area of OtherShape is undefined.")
                    
