type id = string

datatype binop = Add | Mult

datatype prog = Module of stm

and stm = Assign of id * exp
    | CompoundStm of stm * stm
    | PrintStm of exp
    | Expr of exp

and exp = Num of int
         | BinOp of exp * binop * exp
         | Name of id

val p1 = Module (Expr (Num 123));

val p2 = Module (Expr (BinOp (Num 3, Add, Num 2))) : prog;

val p3 = Module (CompoundStm (Assign ("a", Num 3), PrintStm (BinOp (Name "a", Mult, Num 2)))); 
