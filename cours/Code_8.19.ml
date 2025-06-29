let rec remove0 expr =
  match expr with                                       
  | Int _ -> expr
  | Add (Int 0, e) | Add (e, Int 0) -> remove0 e
  | Mul (Int 0, _) | Mul (_, Int 0) -> Int 0
  | Add (e1, e2) -> Add (remove0 e1, remove0 e2)
  | Mul (e1, e2) -> Mul (remove0 e1, remove0 e2)
