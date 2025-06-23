(* the abstract syntax of the source langauge *)
(* contains definitions for expressions *)
structure expressions : EXPRESSIONS = struct 

    (* the possible expressions are type literals, variables, lambdas, applications, let, if and cast expressions *)
    datatype exp = 
        EInt of int
      | EBool of bool
      | EVar of string
      | ELam of string * exp
      | EApp of exp * exp
      | ELet of string * exp * exp
      | EIf of exp * exp * exp
      | ECouple of exp * exp

    (* a function that converts expressions into human readable strings *)
    fun string_of_exp (EInt n) = Int.toString n
      | string_of_exp (EBool b) = Bool.toString b
      | string_of_exp (EVar x) = x 
      | string_of_exp (ELam (x, e)) = 
            "(fun " ^ x ^ " => " ^ string_of_exp e ^ ")"
      | string_of_exp (EApp (e1, e2)) = 
	  		"(" ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ")"
      | string_of_exp (ELet (x, e1, e2)) = 
            "(let " ^ x ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2 ^ ")"
      | string_of_exp (EIf (e1, e2, e3)) = 
            "(if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^ " else " ^ string_of_exp e3 ^ ")"
      | string_of_exp (ECouple (e1, e2)) =
            "(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"

end;