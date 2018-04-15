(* Israel O. Dilan-Pantojas *)
(* 801-11-2035 *)
(* CCOM-4087 *)
(* DePython Interpreter *)


(*Datatypes*)
type id = string

datatype binop = Add | Mult | Div | Sub

datatype prog = Module of stm

and stm = Expr of exp
    | CompoundStm of stm * stm
    | PrintStm of exp
    | Assign of id * exp

and exp = Num of int
         | BinOp of exp * binop * exp
         | Name of id
(*End Datatypes*)

(*Test Cases*)
val p1 = Module (Expr (Num 123));

val p2 = Module (Expr (BinOp (Num 3, Add, Num 2))) : prog;

val p3 = Module (CompoundStm (Assign ("a", Num 3), PrintStm (BinOp (Name "a", Mult, Num 2)))); 

val p4 = Module (CompoundStm (Assign ("b", Num 3), PrintStm (BinOp (Name "b", Add, Num 2))));

val p5 = Module (CompoundStm (Assign ("coso" , Num 5), PrintStm (BinOp(Num 3, Add,BinOp(Num 2, Mult, Name "coso")))));

val p6 = Module (Expr (Name "coso"));

val p7 = Module (CompoundStm (Assign ("coso" , Num 2), PrintStm (BinOp(Num 8, Sub,BinOp(Num 3, Div, Name "coso")))));

(*End Test Cases*)

(*Pretty Printer*)

(*Handle modules*)
fun pretty (Module p) = 
	let  
		(* Handle expressions*)
		fun pretty_exp (Num n) = (Int.toString n)
			| pretty_exp (Name n) = n 
			| pretty_exp (BinOp (n, Add, i)) = pretty_exp(n) ^ " + " ^ pretty_exp(i)
			| pretty_exp (BinOp (n, Mult, i)) = pretty_exp(n) ^  " * " ^ pretty_exp(i)
			| pretty_exp (BinOp (n, Div, i)) = pretty_exp(n) ^  " / " ^ pretty_exp(i)
			| pretty_exp (BinOp (n, Sub, i)) = pretty_exp(n) ^  " - " ^ pretty_exp(i)

		(* Handle Statements*)
		fun pretty_stm (Assign (n,i)) = n ^ " = " ^ pretty_exp(i)
			| pretty_stm (CompoundStm (n,i)) = pretty_stm(n) ^ "\n\t" ^ pretty_stm(i) 
			| pretty_stm (PrintStm (n)) = "print ("^ pretty_exp(n) ^") "
			| pretty_stm (Expr (n)) = pretty_exp(n)

	(*Print padding and call function to return statment string to be printed*)
	in print ("\n\t" ^ pretty_stm(p) ^ "\n\n")
	end;
(*End Pretty Printer*)

(*Run test cases*)
pretty(p1);

pretty(p2);

pretty(p3);

pretty(p4);

pretty(p5);

pretty(p6);

pretty(p7);
(*End run test cases*)

(*DePython Interpreter*)
(*Interpreter Function *)
fun interp (Module p) = 
	let
		(*Define a table list for storing variables as id and values as int*)
		type table = (id * int) list

		(*Lookup variable value by name(id)*)
		fun lookup ([]: table, s: id): int = raise  (Fail "Unbound Identifier")
			| lookup (((i,n)::t): table, s) = (if i <> s then lookup(t,s) else n)

		(*Update variable value*)
		fun update ([] : table, n: id, i: int): table = [(n,i)]
			| update (t: table , n: id , i: int) =  [(n,i)]@t

		(* Not used *)
		(*Actually updates variable value*)
		fun update2 ([] : table, n: id, i: int): table = []@[(n,i)]
			| update2 (head::tail: table , n: id , i: int) = let val (x,q) = head
									 in  if x <> n then [head]@update2(tail,n,i)
									     else [(n,i)]@tail 
									 end
		(* Not used *)

		(* Interpret expressions *)
		fun interp_exp (tab, (Num n)) = (n)
			| interp_exp (tab, (Name n)) = (lookup(tab, n))
			| interp_exp (tab, (BinOp (n, Add, i))) = ((interp_exp(tab, n)) + (interp_exp(tab, i)))
			| interp_exp (tab, (BinOp (n, Mult, i))) = ((interp_exp(tab, n)) * (interp_exp(tab, i)))
			| interp_exp (tab, (BinOp (n, Div, i))) = ((interp_exp(tab, n)) div (interp_exp(tab, i)))
			| interp_exp (tab, (BinOp (n, Sub, i))) = ((interp_exp(tab, n)) - (interp_exp(tab, i)))


		(* Interpret statements *)
		fun interp_stm (tab, (Assign (n,i))) = (update(tab, n, interp_exp(tab, i)))
			| interp_stm (tab, (Expr (n))) =  (interp_exp(tab, n); tab)
			| interp_stm (tab, (CompoundStm (n,i))) = (interp_stm(interp_stm(tab, n), i))
			| interp_stm (tab, (PrintStm (n))) = (print ( "\t" ^ Int.toString(interp_exp(tab, n)) ^"\n"); tab) 

	(* Print some padding and call function to interpret statements *)
	in print ("\n"); interp_stm([]:table, p) ; print ("\n") 
	end;
(*End DePython Interpreter*)

(*Run test cases*)
interp(p1);

interp(p2);

interp(p3);

interp(p4);

interp(p5);

(*interp(p6);*)
(*P6 Raises error as instructed*)
(*End run test cases*)

interp(p7);

(* Bonus *)
print("\n\n Print and evaluate P7 \n");
pretty(p7); interp(p7); 
(* Bonus *)

