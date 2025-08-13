(* the abstract syntax of the source langauge *)
(* contains definitions for expressions *)
structure expressions : EXPRESSIONS = struct 

    (* the possible expressions are type literals, variables, lambdas, applications, let, if and cast expressions 
    two more instructions used mainly for testing are the Plus1 and the Negation*)
    datatype exp = 
        EInt of int
      | EBool of bool
      | EVar of string
      | EPlus1 of exp
      | ENeg of exp
      | ELam of string * exp
      | EApp of exp * exp
      | ELet of string * exp * exp
      | EIf of exp * exp * exp
      | EPair of exp * exp

    (* a function that converts expressions into human readable strings *)
    fun exp_to_string (EInt n) = Int.toString n
      | exp_to_string (EBool b) = Bool.toString b
      | exp_to_string (EVar x) = x 
      | exp_to_string (EPlus1 e) = 
            "(" ^ exp_to_string e ^ " + 1)"
      | exp_to_string (ENeg e) =
            "(!" ^ exp_to_string e ^ ")"
      | exp_to_string (ELam (x, e)) = 
            "(fun " ^ x ^ " => " ^ exp_to_string e ^ ")"
      | exp_to_string (EApp (e1, e2)) = 
	  		"(" ^ exp_to_string e1 ^ " " ^ exp_to_string e2 ^ ")"
      | exp_to_string (ELet (x, e1, e2)) = 
            "(let " ^ x ^ " = " ^ exp_to_string e1 ^ " in " ^ exp_to_string e2 ^ ")"
      | exp_to_string (EIf (e1, e2, e3)) = 
            "(if " ^ exp_to_string e1 ^ " then " ^ exp_to_string e2 ^ " else " ^ exp_to_string e3 ^ ")"
      | exp_to_string (EPair (e1, e2)) =
            "(" ^ exp_to_string e1 ^ ", " ^ exp_to_string e2 ^ ")"

end;