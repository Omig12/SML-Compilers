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

val p4 = Module (Expr (Name "coso"));

val p5 = Module (CompoundStm (Assign ("b", Num 3), PrintStm (BinOp (Name "b", Add, Num 2))));

val p6 = Module (CompoundStm (Assign ("coso" , Num 5), PrintStm (BinOp(Num 3, Add,BinOp(Num 2, Mult, Name "coso")))));


fun pretty (Module p) = 
	let
		fun pretty_exp (Num n) = (Int.toString n)
			| pretty_exp (Name n) = n 
			| pretty_exp (BinOp (n, Add, i)) = pretty_exp(n) ^ " + " ^ pretty_exp(i)
			| pretty_exp (BinOp (n, Mult, i)) = pretty_exp(n) ^  " * " ^ pretty_exp(i)

		fun pretty_stm (Assign (n,i)) = n ^ " = " ^ pretty_exp(i)
			| pretty_stm (CompoundStm (n,i)) = pretty_stm(n) ^ "\n\t" ^ pretty_stm(i) 
			| pretty_stm (PrintStm (n)) = "print ("^ pretty_exp(n) ^") "
			| pretty_stm (Expr (n)) = pretty_exp(n)

	in print ("\n\t" ^ pretty_stm(p) ^ "\n\n")
	end;

pretty(p1);

pretty(p2);

pretty(p3);

pretty(p4);

pretty(p5);

pretty(p6);