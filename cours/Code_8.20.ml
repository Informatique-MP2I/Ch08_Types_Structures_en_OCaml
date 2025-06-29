type expr =
  | Int of int 
  | Add of expr * expr
  | Mul of expr * expr
  | F of func  
and func =
  | Fact of expr
  | Square of expr
